
draw_confusion_matrix <- function(cm, c1_name, c2_name, title="CONFUSION MATRIX") {

    ### taken from https://stackoverflow.com/questions/23891140/r-how-to-visualize-confusion-matrix-using-the-caret-package

    total <- sum(cm$table)
    res <- as.numeric(cm$table)

    # Generate color gradients. Palettes come from RColorBrewer.
    greenPalette <- c(
    "#F7FCF5","#E5F5E0","#C7E9C0",
    "#A1D99B","#74C476","#41AB5D",
    "#238B45","#006D2C","#00441B"
    )
    redPalette <- c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
    getColor <- function (greenOrRed = "green", amount = 0) {
    if (amount == 0)
        return("#FFFFFF")
    palette <- greenPalette
    if (greenOrRed == "red")
        palette <- redPalette
    colorRampPalette(palette)(100)[10 + ceiling(90 * amount / total)]
    }

    # set the basic layout
    layout(matrix(c(1,1,2)))
    par(mar=c(2,2,2,2))
    plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
    title(title, cex.main=2)

    # create the matrix 
    classes = colnames(cm$table)
    rect(150, 430, 240, 370, col=getColor("green", res[1]))
    text(195, 435, c1_name, cex=1.2)
    rect(250, 430, 340, 370, col=getColor("red", res[3]))
    text(295, 435, c2_name, cex=1.2)
    text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
    text(245, 450, 'Actual', cex=1.3, font=2)
    rect(150, 305, 240, 365, col=getColor("red", res[2]))
    rect(250, 305, 340, 365, col=getColor("green", res[4]))
    text(140, 400, c1_name, cex=1.2, srt=90)
    text(140, 335, c2_name, cex=1.2, srt=90)

    # add in the cm results
    text(195, 400, res[1], cex=1.6, font=2, col='white')
    text(195, 335, res[2], cex=1.6, font=2, col='white')
    text(295, 400, res[3], cex=1.6, font=2, col='white')
    text(295, 335, res[4], cex=1.6, font=2, col='white')

    # add in the specifics 
    plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
    text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
    text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
    text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
    text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
    text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
    text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
    text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
    text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
    text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
    text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)

    # add in the accuracy information 
    text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
    text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
    text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
    text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)

}  
