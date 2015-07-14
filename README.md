# Supreme

`Supreme` is a set of useful functions for my PhD thesis on applying LDA topic models to a corpus of Italian Supreme Court decisions. It's built on **tm** and **topicmodels** packages and its development is currently 50% complete.

It has three main goals:

- Make it easier applying the topic model framework to a corpus-based legal terminology.

- Provide tools, based on the topic model framework, for classifying documents and for retrieving similar documents to a given document. 

- Provide utilities for fast performance by using parallel computing.

A topic model is a generative model that specifies a simple probabilistic procedure by which documents in a corpus can be generated. In the **L**atent **D**irichlet **A**llocation approach, a topic is a probability distribution over a fixed vocabulary of terms, each document is modeled as a mixture of **k** topics, and the mixing coefficients can be used for representing the documents as points on k-􀀀1 dimensional simplex spanned by the topics. Approximate posterior inference is performed in order to learn the hidden topical structure from the observed data, i.e., the words in the documents.

LDA is the simplest topic model and many more models can be built from it.

Full reference can be found in:

1. D.M. Blei and J.D. Lafferty, Text mining: Classification, clustering, and applications, ch. Topic Models, Chapman & Hall/CRC Press, 2009.

2. D.M. Blei and A.Y. Ng and M.I. Jordan, Latent Dirichlet Allocation, Journal of Machine Learning Research, 3, 2003, 993-1022 (http://machinelearning.wustl.edu/mlpapers/paper_files/BleiNJ03.pdf).

3. T.L. Griffiths and M. Steyvers, Finding scientific topics, Proceedings of the National Academy of Sciences of the United States of America, vol. 101, National Academy of Sciences, 2004, Suppl. 1, pp. 5228–5235.

You can install the latest development version from github with:

    ``` r
    install.packages("devtools")
    devtools::install_github("paolofantini/Supreme")
    ```
    
## Input data

### Data frame of class dfciv

A data frame with **15,485** rows and **15** variables relating to final decisions (sentences) in civil matters delivered by the **I**talian **S**upreme **C**ourt during the year 2013.

#### Description of the variables

- *Id_doc*: ID document

- *tipoProv*: type of decision (D, I, O, S) - S stands for *Sentenza* (final decision)
 
- *annoDec*: year of decision

- *numDec*: number of decision

- *numSez*: number of ISC section

- *testo*: **text** of decision

- *dispositivo*: summary of decision

- *annoNrgSic*: year of filing lawsuit

- *nrgSic*: number of filing lawsuit

- *annoProvOrig*: year of appealed decision 

- *numProvOrig*: number of appealed decision

- *autorita*: authority of appealed decision

- *localita*: location of appealed decision

- *materia*: subject of appealed decision

- *Idmateria*: ID subject of appealed decision

You can obtain the data frame **dfciv**:   

1. loading the compressed xml file from [here](https://www.dropbox.com/s/t0eiqagdyd67ei7/ISC_Civil_2013.zip?dl=0)

2. and running the following code

``` r
library(Supreme)

# Unzip and load in memory the xml input file. 
xml_input <- unzip("../ISC_Civil_2013.zip")
dfciv_original <- xmlciv2df(xml_input)

# Select the subset of final decisions. 
dfciv_final <- subset(dfciv_original, tipoProv == "S")

# Select only the 10 larger classes.
cl <- names(sort(table(dfciv_final$Idmateria), decreasing = TRUE))[1:10]
dfciv <- subset(dfciv_final, Idmateria %in% cl)

str(dfciv)

rm(dfciv_original, dfciv_final)

```

### Labels: classes

Labels that are assigned to cases when they come to the Supreme Court from lower courts. 

We consider only the first 10 larger classes:

- *class 71*: Tributi (taxes)
- *class 42*: Lavoro (employment)
- *class 30*: Equa riparazione (equitable relief)
- *class 16*: Contratti: tutti gli altri tipi (contracts: other types)
- *class 15*: Contratti e obbligazioni in genere (contracts and obligations in general)
- *class 57*: Responsabilita' civile (civil liability)
- *class 49*: Previdenza (social security)
- *class 19*: Diritti reali (property rights)
- *class 34*: Fallimento e istituti affini (bankruptcy)
- *class 74*: Vendita, Permuta, Riporto (sale, exchange, carry-over)

## Functions

`Supreme` implements the following functions:

- `xmlciv2df():`transforms the original xml documents to a **dfciv** data frame.

- `dfciv2corpus():`creates an *ISC* corpus from a **dfciv** data frame.

- `corpus2dtm():`transforms an *ISC* corpus to a document term matrix **dtm**.

- `reduce_dtm_tfidf():`selects suitable columns (terms) of an **unlabeled** document-term matrix by deleting terms whith tf-idf score out of an user defined range.

- `reduce_dtm_lognet():`selects suitable columns of an **labeled** document-term matrix by applying the *lognet* method (logistic classification) from package **glmnet**.

- `reduce_dtm():`a simple wrapper for `reduce_dtm_tfidf()` and `reduce_dtm_lognet()`.

- `logClass():`fits logistic classification models from **glmnet** and **caret** packages to the *topic.posteriors* matrix of predictors trained by LDA topic model.
A `target` variable is used as a supervision variable.

- `mcLDA():`runs multiple parallel LDA models by varying the number of topics `k` over a predefined grid of values
and performs model selection based on logistic classification by calling `logClass()` function.

## Output data

### Hard document-term matrix

A document-term matrix **dtm** with **15,485** rows and **52,504** columns.
The rows in this matrix correspond to the documents and the columns to the terms.
The entry $f_{i,j}$ indicates the frequency of *j-*th term in the *i-*th document.
The number of rows is equal to the size of the corpus and the number of columns to the size of the vocabulary.

**dtm** contains the *original* document-term matrix as obtained by applying the `corpus2dtm()` function to the original **ISC corpus**.
Each row in this matrix represents a document as a simple *bag of words* after removing punctuation, numbers, stopwords and white spaces.

You can obtain the original **corpus** and the hard **dtm** by running the following code:

``` r
library(Supreme)

# Get (and save) the corpus from data frame dfciv.
data("dfciv")
corpus <- dfciv2corpus(dfciv, TRUE)

# Get dtm from corpus. 
data("italianStopWords")  # for removing italian stop words
dtm <- corpus2dtm(corpus)

```

### Dimensionality reduction of (hard) dtm

`reduce_dtm()` reduces the number of columns (terms) of **dtm** by applying two different methods:

- **tfidf:** selects suitable columns of *unlabeled* **dtm** by deleting terms whith [tf-idf](http://en.wikipedia.org/wiki/Tf-idf) score out of an user defined range. By default, this reduces the number of terms by half from 52,504 to **26,252**.

- **lognet:** applies to *labeled* **dtm** the `lognet` method, a logistic classification method from package **glmnet**: **dtm** can be represented as a *labeled* document-term matrix by using the class labels **classes**, i.e. the labels assigned to cases when they come to the Supreme Court from lower courts. This dramatically reduces the vocabulary to **798** *useful* terms.

You can obtain the reduced document-term matrices **dtm.tfidf** and **dtm.lognet** by running the following code:

``` r
library(Supreme)

### tfidf method
data("dtm")
dtm.tfidf <- reduce_dtm(dtm, method = "tfidf", export = TRUE)

### lognet method
data("dtm")
data("classes")
dtm.lognet <- reduce_dtm(dtm, method = "lognet", target = classes, export = TRUE)

```

### Multicore parallel runs of LDA models and best model selection

`mcLDA()` runs multiple parallel **LDA models** by varying the number of topics **k** over a predefined
grid of values and performs model selection by applying the `logClass()` function to each model.
A vector of misclassification error on the test set (`e1.test`) is returned and the best model is selected with minimum misclassification error.

In the following example, we run **8** different models in parallel on a **8**-cores CPU by assigning one model to each core.

``` r
library(Supreme)

data("dtm")
data("classes")
dtm.lognet <- reduce_dtm(dtm, method = "lognet", target = classes)

# 8 cores: one model for each core.
mc.lda.models <- mcLDA(dtm.lognet$reduced, lda.method = "VEM", k.runs = list(from = 10, to = 45, steps = 5), target = classes)

```

`mcLDA()` uses parallel computing functionalities provided by packages **parallel**, **doParallel** and **foreach**.
They should work fine on both Unix-like and Windows-like systems.
