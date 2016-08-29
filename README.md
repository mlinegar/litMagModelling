
<!-- README.md is generated from README.Rmd. Please edit that file -->
litMagModelling
===============

This package is designed to be an easy interface to MALLET, and is aimed in particular at digital humanists. This package was developed alongside a forthcoming digital humanities paper, which will be linked to once it has been published. It is composed of two main sections: tools to go from a folder of text files to a MALLET corpus, and tools for graphing the resulting LDA models.

Data
----

The data used in this README can be found [here](http://sourceforge.net/projects/mjplab/files/). After loading the package, a vector of publication dates and a `data.frame` of contributors have been provided for the following journals: Freewoman, New Freewoman, Egoist, Blast, and Poetry Magazine. The function `strip_dates` can be used to automatically create a vector of dates from a folder of XML or txt files, though it has only been tested on the five journals previously mentioned.

Getting started
---------------

After downloading the desired data and storing all of the files in a single folder (let's call it `dataFolder`), go ahead and download this package. You'll need devtools to do so:

    install.packages("devtools")
    library(devtools)
    # it's good practice to download the package every time, though, so it'll automatically pick up updates
    install_github("mlinegar/litMagModelling")
    library(litMagModelling)

And now let's set our initial parameters:

    dataFolder <- "~/Path/to/dataFolder"
    # pattern is the pattern ending of the files in dataFolder. Defaults to '.tei.xml', as that is the default for the SourceForge files. Here I've set it to '.txt' as I've already stripped out the xml/html. 
    pattern <- ".txt"
    # number of LDA topics
    ntopics <- 50
    # number of top words to include when looking at each topic
    numtopwords <- 7

Let's also set some stop words - this is in addition to automatically stripping out a small amount of xml, as well as the stop-word lists from the `tm` package for English, French, and German:

    badWords <- c("co", "mr", "mrs", "th", "hed", "em", "wm", "phe", "pthe", "lg", "pi", "ty",
                  "si", "vo", "op", "wc", "mr", "yer", "spitteler", "emer", "tarr", "nero")
    commonTerms <- c("september", "october", "november", "december", "january",
                     "february", "march", "april", "may", "june", "july", "august")
    properNames <- c("walt", "stephen", "diomedes", "martin", "cunningham", "bertha", 
                     "emanuel", "kreisler")
    wordsToRemove <- c(letters, badWords, commonTerms, properNames, unique(contributors$firstName), unique(contributors$lastName))

Next, we load the data and create our model:

    marz <- prep_corpus(dataFolder, date_vec = date_vec, pattern = pattern, journalVec = c("Blast", "Egoist", "NewFreewoman", "Freewoman", "Poetry Magazine"), yearRangeRule = "< 1920")
    # note that this stopListFile for make_model has VERY particular restrictions
    # it must be a .txt file, not a CSV, it can have no blank lines, and no punctuation
    trained_marz <- make_model(marz, n.topics = ntopics, stopListFile = "~/Path/to/stopwords.txt")
    # now we convert to a data.frame for easy graphing
    trained_marz.df <- modelToDataframe(trained_marz, marz, num.topwords = numtopwords)

Finally, we're ready to graph our models. There are two easy graphs that this package creates: the first, using `graph_model`, creates one graph for each topic, and plots the progression of topics over time, broken down by journal. The function automatically sets the maximum number of graphs to display per page to ten, but this number can be changed. If the number of topics is greater than ten, multiple pages of graphs will be created. The second graph that `litMagModelling` can produce is a LDA plot from `LDAvis`. I introduced this function because I had had some difficulty creating this plot by hand. If the function runs for more than a few minutes, there is likely an error in the function `LDAvis::createJSON()` due to some problem in the stop-word file (for example, there might be commas in the file, or empty lines).

    graph_model(trained_marz.df)
    # you can save this to a gist if desired
    # note that to exit, you must hit the STOP button
    makeLDAvis(trained_marz.df)
