#!/bin/bash
# borrowed with modifications from https://gist.github.com/waylan/4080362


# ====================================================================================
# === preamble
# ====================================================================================

PROG_NAME=$(basename $0)

# determine colors support (https://unix.stackexchange.com/a/10065)
# check if stdout is a terminal...
if test -t 1; then
    # see if it supports colors...
    ncolors=$(tput colors)

    if test -n "$ncolors"; then
        if test $ncolors -ge 8; then
            bold="$(tput bold)"
            underline="$(tput smul)"
            standout="$(tput smso)"
            normal="$(tput sgr0)"
            black="$(tput setaf 0)"
            red="$(tput setaf 1)"
            green="$(tput setaf 2)"
            yellow="$(tput setaf 3)"
            blue="$(tput setaf 4)"
            magenta="$(tput setaf 5)"
            cyan="$(tput setaf 6)"
            white="$(tput setaf 7)"
        fi
        if test $ncolors -ge 9; then
            gray="$(tput setaf 8)"
        fi
    fi
fi
# end color support

sub_help() {
    echo "Usage: $PROG_NAME <subcommand> [options]"
    echo "Subcommands:"
    echo "    figures   Generate figures from the paper; usage: $PROG_NAME figures [--rerun-bootstrap] <figure_name|clean>"
    echo "               --rerun-bootstrap: recomputes bootstrap estimates from scratch (expensive)"
    echo "               figure_name: recreates a specific figure, or if 'all' is specified all of them."
    echo "                 available options: figure1, figure2, figure3, figure4, supplemental1, all"
    echo "               clean: removes all intermediate and final figure artifacts, and the bootstrap data cache"
    echo ""
    echo "    shell     Creates a (bash) shell within the environment"
    echo ""
}


# ====================================================================================
# === 'shell' subcommand
# ====================================================================================

sub_shell() {
    bash
}


# ====================================================================================
# === 'figures' subcommand
# ====================================================================================

build_figure1() {
    echo "*** Building figure 1..."
    Rscript -e "library(rmarkdown); rmarkdown::render('figure1.Rmd')"
    return $?
}
build_figure2() {
    if [[ $1 -eq 1 ]]; then
        rm -f /app/figure_notebooks/manuscript_figs/fig2_tmp/fig2.RData
    fi
    echo "*** Building figure 2..."
    Rscript -e "library(rmarkdown); rmarkdown::render('figure2.Rmd')"
    return $?
}
build_figure3() {
    if [[ $1 -eq 1 ]]; then
        rm -f /app/figure_notebooks/manuscript_figs/fig3_tmp/all_bootstrap_df.tsv
    fi
    echo "*** Building figure 3..."
    Rscript -e "library(rmarkdown); rmarkdown::render('figure3.Rmd')"
    return $?
}
build_figure4() {
    if [[ $1 -eq 1 ]]; then
        rm -f /app/figure_notebooks/manuscript_figs/fig4_tmp/*.tsv
    fi
    echo "*** Building figure 4..."
    Rscript -e "library(rmarkdown); rmarkdown::render('figure4.Rmd')"
    return $?
}
build_supplemental1() {
    echo "*** Building supplemental 1..."
    Rscript -e "library(rmarkdown); rmarkdown::render('supp_fig1.Rmd')"
    return $?
}

sub_figures() {    
    RERUN_BOOTSTRAP=0
    if [[ $1 == '--rerun-bootstrap' ]]; then
        shift
        RERUN_BOOTSTRAP=1
    fi

    # double check that they want to do a destructive operation
    if [[ $RERUN_BOOTSTRAP -eq 1 || $1 == 'clean' ]]; then
        read -r -p "delete and re-create temporary files? (this is very expensive, maybe >11 hrs per figure) [y/N]" response
        case "$response" in
            [yY][eE][sS]|[yY])
                # pass
                ;;
            *)
                echo "aborting file destruction..."
                exit 1
                ;;
        esac
        echo "Clearing and recreating cached bootstrap data; recomputing may take several hours..."
    fi


    # # in order to use git lfs, we need an https remote, not ssh; make sure that it's set now
    # OLD_ORIGIN=$( git remote get-url origin )
    # git remote set-url origin https://github.com/greenelab/nature_news_disparities.git
    # git checkout data/scraped_data
    # git remote set-url origin "${OLD_ORIGIN}"
    # ...unfortunately, the above is *very* slow and prone to nuking the index and/or working dir
    # instead we'll just prompt the user to git checkout on their own if we detect they need to

    if ! compgen -G "/app/data/scraped_data/coreNLP_output_*" > /dev/null || ! compgen -G "/app/data/scraped_data/*.tsv"  > /dev/null; then
        echo "${red}ERROR:${bold} Figure generation requires that /app/data/scraped_data is populated with data from the repo + LFS${normal}"
        echo "Please run git checkout /app/data/scraped_data from within the container before attempting to generate figures"
        echo "(Note that you must also have your origin remote's url set to https://github.com/greenelab/nature_news_disparities.git)"
        exit 1
    fi

    pushd /app/figure_notebooks 2>&1 >/dev/null

    case $1 in
        "figure1")
            build_figure1 $RERUN_BOOTSTRAP
            ;;
        "figure2")
            build_figure2 $RERUN_BOOTSTRAP
            ;;
        "figure3")
            build_figure3 $RERUN_BOOTSTRAP
            ;;
        "figure4")
            build_figure4 $RERUN_BOOTSTRAP
            ;;
        "supplemental1")
            build_supplemental1 $RERUN_BOOTSTRAP
            ;;
        "all")
            tasks=(
                "figure 1       "
                "figure 2       "
                "figure 3       "
                "figure 4       "
                "supplemental 1 "
            )
            results=()
            build_figure1 $RERUN_BOOTSTRAP; results+=( $? )
            build_figure2 $RERUN_BOOTSTRAP; results+=( $? )
            build_figure3 $RERUN_BOOTSTRAP; results+=( $? )
            build_figure4 $RERUN_BOOTSTRAP; results+=( $? )
            build_supplemental1 $RERUN_BOOTSTRAP; results+=( $? )

            echo ""
            echo "Results:"
            for (( i=0; i<${#results[@]}; i++ )); do
                echo "- ${bold}${tasks[$i]}${normal} :" $( test ${results[$i]} -eq 0 && echo "${green}pass${normal}" || echo "${red}fail${normal} ${gray}(code: ${results[$i]})${normal}" )
            done
            echo ""

            ;;
        "clean")
            for target in figure1_files figure2_files figure3_files figure4_files supp_fig1_files tmp_files; do
                find "/app/figure_notebooks/$target" -type f -exec rm -f {} \;
            done
            ;;
        *)
            echo "Usage: $PROG_NAME figures [--rerun-bootstrap] <figure_name|clean>"
            echo "  --rerun-bootstrap: recomputes bootstrap estimates from scratch (expensive)"
            echo "  figure_name: recreates a specific figure, or if 'all' is specified all of them."
            echo "    available options: figure1, figure2, figure3, figure4, supplemental1, all"
            echo "  clean: removes all intermediate and final figure artifacts, and the bootstrap data cache"
            echo ""
            exit 1
            ;;
    esac

    popd 2>&1 >/dev/null
}


# ====================================================================================
# === subcommand parser
# ====================================================================================

subcommand=$1
case $subcommand in
    "" | "-h" | "--help")
        sub_help
        ;;
    *)
        shift # get next arguement, $1 now has the next entry
        # check if it exists first
        type -t sub_${subcommand} 2>&1 >/dev/null
        if [ $? != 0 ]; then
            echo "Error: '$subcommand' is not a known subcommand." >&2
            echo "       Run '$PROG_NAME --help' for a list of known subcommands." >&2
            exit 1
        fi
        # actually run it
        sub_${subcommand} $@
        ;;
esac
