library(ggplot2)

(p <- ggplot(aes(y = Freq, x = threshold, colour = lab, group = lab), data = df)
    + geom_line()

)


