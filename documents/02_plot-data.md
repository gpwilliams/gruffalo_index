Gruffalo Plots
================

Corpus Analysis: The Gruffalo and The Gruffalo's Child
======================================================

In this post, we'll conduct a corpus analysis for The Gruffalo and The Gruffalo's Child (both by Julia Donaldson), looking at how their Standard British versions vary from their Scots translations (performed by various translators at Itchy-Coo).

This was prompted by a series of studies I'm running with Vera Kempe and Nikolay Panayotov at Abertay University. In these studies, we're exploring whether exposure to multiple languages/language varieties helps or hinders literacy acquisition using an artificial language learning task. (Spoiler for future posts, it may help!) Before we got started with the study, we had to design a miniature artifical language with multiple varieties (some real examples could be Standard British English and Geordie or differences between many Scots varieties).

To get an idea for how languages can vary, we used the Gruffalo books to get an idea of how often different language varieties differ, and how they differ when they do. This led me to copy the Standard British English versions of the Gruffalo and The Gruffalo's Child, along with the Scots, Dundonian, Glaswegian, and Doric versions of these (excluding the Glasgow Gruffalo's Child, as this wasn't released at the time).

Our analyses based on this corpus informed how we built our miniature artificial language. In this post, we'll explore how the varieties differ across the Gruffalo books. We'll use R, primarily relying on the tidyverse family of functions to perform our analyses. You can skip this code if you just want the results, though, so please read on even if R isn't your thing!

Analysis
========

We'll use a handful of packages for our exploration of the data. I often use the `here` package which allows us to read and write data from reative file paths across machines. Why should you use relative rather than absolute file paths? [Jenny Bryan will set your computer on fire if you use the latter](https://github.com/jennybc/here_here). The `tidyverse` package is the main workhorse that allows us to do most of our data processing and plotting. Next, we use `cowplot` which allows us to include images in our plots (amongst other cool features), and `ggridges` which lets us easily plot some cool ridgeline plots (see the [ggridges GitHub page](https://github.com/clauswilke/ggridges) for a nice example on how to use this pacakge).

``` r
packages <- c(
  "here",
  "tidyverse",
  "cowplot",
  "magick",
  "ggridges",
  "ggwordcloud"
)

# apply library to all names in packages
lapply(packages, library, character.only = TRUE)
```

Read and Prepare the Raw Data
-----------------------------

We'll first read in the data using `readr::read_csv()`. This is much like `read.csv()` from base R, but with some added bells and whistles. Primarily, I use `read_csv` over the base equivalent for its speed, because it notifies you about how data types are stored, and it saves data in a tibble. (If you want to know why you might prefer tibbles over data frames, check out the [tibble package vignette](https://cran.r-project.org/web/packages/tibble/vignettes/tibble.html).) Finally, we use `ggwordcloud` for plotting wordclouds for the type frequencies.

``` r
token <- read_csv(here("data", "02_processed-data", "token_freq.csv"))
type <- read_csv(here("data", "02_processed-data", "type_freq.csv"))
```

Let's take a look at our data. Just how are the token and type data organised?

As we can see, the token data contains several columns related to the books in question, which informant (translator) translated the books, and counts of where each English word occurred in the book (1 being the first word, 2 the second, etc.). You can see that we have some missing data here. This is because the English and Scots phrases couldn't always be aligned; to fit the story to a rhyme using Scots words, sometimes entire phrases had to be replaced from the English text, so we don't count this in our analysis. Still, we take a running count of how many English words were traslated into Scots. Next to the English and Scots versions of the words, we also have a number of variables measuring different things. We have a binary code for whether the Scots word is different to the English word lexically or phonologically (or if we were unsure and couldn't categorise it as lexical or phonological). These all take 0 if the English and Scots words don't differ on these measures, and 1 if they do.

Next, we have shift type, which describes how words changed (if they did). We can see that, e.g. the English "foot" and Scots "fit" differ by a vowel shift. We also have an indication of where in the word these changes occurred (e.g. onset, nucleus, coda). Note that these measures are only tracked for phonological shifts.

Next, we have CLAWS tags -- tags from the English words taken from the [CLAWS part-of-speech tagger for English](http://ucrel.lancs.ac.uk/claws/), which gives us fine-grained part of speech categories for the English words. We also have our own, broad categories informed by these tags, under the shift category column.

Finally, we have some objective measures of the difference between the English and Scots words. This is the Levenshtein Edit Distance (LED), which is a count of the number of insertions, subtitutions, and deletions needed to transform the English word to the Scots word. Using the maximum word length from the two words (e.g. English "foot" and Scots "fit" = 4), we can figure our the length-normalised LED, nLED, which shows less variability in scores by extreme word lengths.

``` r
glimpse(token)
```

    ## Observations: 3,458
    ## Variables: 18
    ## $ book                    <chr> "doric_child", "doric_child", "doric_c...
    ## $ informant               <chr> "doric", "doric", "doric", "doric", "d...
    ## $ text_count              <dbl> 1, 2, 3, 10, 11, 12, 13, 14, 15, 16, 1...
    ## $ running_count           <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,...
    ## $ eng                     <chr> "the", "gruffalo", "said", "set", "foo...
    ## $ scots                   <chr> "the", "gruffalo", "quo", "set", "fit"...
    ## $ diff                    <dbl> 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1,...
    ## $ lex                     <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1,...
    ## $ phon                    <dbl> 0, 0, NA, 0, 1, 0, 0, 0, 1, 1, NA, NA,...
    ## $ unsure                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
    ## $ shift_type              <chr> NA, NA, NA, NA, "vowel_shift", NA, NA,...
    ## $ syllable_shift_location <chr> NA, NA, NA, NA, "nucleus", NA, NA, NA,...
    ## $ CLAWS_tag               <chr> "AT0", "NN1", "AJ0", "AJ0", "NN1", "AV...
    ## $ type                    <chr> "FUN", "PROPER", "VERB", "ADJ", "NOUN"...
    ## $ shift_category          <chr> "unchanged", "unchanged", "lexical", "...
    ## $ LED                     <dbl> 0, 0, 4, 0, 2, 0, 0, 0, 1, 2, 3, 2, 3,...
    ## $ max_length              <dbl> 3, 8, 4, 3, 4, 2, 3, 4, 4, 4, 3, 3, 3,...
    ## $ nLED                    <dbl> 0.00, 0.00, 1.00, 0.00, 0.50, 0.00, 0....

So, first off, we want to make this data a little more readable by having nicer names for the parts-of-speech under the `type` header. We can use the `dplyr::case_when()` function for this. This is a nice way to change things by conditions when we have many conditions in our data (avoiding many `if()`, `else()`, or `ifelse()` commands).

``` r
token <- token %>%
  mutate(
    type = case_when(
      type == "FUN" ~ "function",
      type == "PROPER" ~ "proper noun",
      type == "VERB" ~ "verb",
      type == "ADJ" ~ "adjective",
      type == "NOUN" ~ "noun",
      type == "ADV" ~ "adverb",
      type == "AUX" ~ "auxiliary",
      type == "CONTR" ~ "contraction"
    )
  ) %>% 
  mutate(type = as.factor(type)) # enforces ordering for colours in plots
```

Token Frequencies
-----------------

First off, we'll explore the token frequencies for the words. Remember, this just takes a measure of how often things changes across all occurrences of words. This means that words that are very common get counted in the data a lot, so highly frequent words contribute a lot towards our measures.

### Exploring Shifts

#### Proportions of Shifts by Category

We'll take a look at how often words change, and when they do, how they change. For this, we can create a simple donut plot to get a coarse measure of our categories.

First off, we'll figure out the proportion of changes in the English words when translated to Scots for four categories: lexical shifts, phonological shifts, unchanged words, and cases where we see a shift, but we can't confidently categorise it (unsure).

``` r
donut_data <- token %>% 
  group_by(shift_category) %>% 
  summarise(
    n = length(nLED), 
    total = nrow(.), 
    proportion = length(nLED)/nrow(.)
  )

# add additional columns to get full coordinates for each category
# used for the donut plot
donut_data$ymax = cumsum(donut_data$proportion)
donut_data$ymin = c(0, donut_data$ymax[1:length(donut_data$ymax)-1])
```

Next, we'll plot the data. Notice that we annotate the data by using our function to figure our the position of the labels for each category in our summary of the data. We then add these labels to the plot using `annotate()`. Finally, we use functions from `cowplot` to a picture of the Gruffalo and his child to the plot.

``` r
# here we map an anonymous function to do row-wise computation of medians
# this finds the location of labels for the donut plot
donut_annotations <- tibble(
  labels = c("lexical", "phonological", "unchanged", "unsure"),
  y = pmap_dbl(donut_data, function(ymin, ymax, ...) median(c(ymin, ymax)))
)

donut_plot <- donut_data %>%
ggplot(aes(fill = shift_category, ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
  geom_rect(colour = "grey30") +
  coord_polar(theta = "y") +
  xlim(c(0, 4)) +
  theme_void() +
  annotate(
    "text", 
    x = 3.5, 
    y = donut_annotations$y, 
    label = donut_annotations$labels,
    size = 7.2
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
    ) +
  scale_fill_viridis_d(option = "viridis", alpha = 0.7)

# add the gruffalo picture to the plot
ggdraw(donut_plot) + 
  draw_image(here("img", "gruffalo_child.png"), scale = 0.45)
```

![](C:/Users/g517364/Dropbox/GitHub/gruffalo_index/documents/02_plot-data_files/figure-markdown_github/donut_plot-1.png)

We can see that while the biggest category individually is the unchanged one, taken together we still have more differences in words between the Scots and English versions of the books. But when a change occurs, it is most often a phonological change (e.g. "foot" to "fit") rather than a lexical one (e.g. "child" to "bairn"). Luckily, we have few cases where we're unsure about the categories, which just means I'm not as incompetent at coding as you might imagine.

Next, we'll get a summary of these proportions, so we can be sure how often a change occurs without resorting to the plot.

``` r
token_contrastive <- tibble(
  contrastive = donut_data %>% 
    filter(shift_category %in% c("lexical", "phonological")) %>% 
    pull(n) %>% 
    sum(),
  total = donut_data$total %>% unique()
) %>%
  mutate(proportion = contrastive/total)
```

We can see that across all occurrences of words (token frequency), contrastive words (i.e. those with a lexical/phonological variant) occured 53.01% of the time.

Phonological shifts occur 33.89% of the time across all books. When a shift occurs, this shift is phonological (rather than lexical) 63.94% of the time.

#### Phonological Shifts by Part of Speech

Next, we'll explore which parts of speech are particularly affected by lexical or phonological shifts when a word is translated from English to Scots. We'll rely on `dplyr` here to filter the words to phonological shifts only, before calculating summaries by part of speech.

``` r
token_phon_descriptives <- token %>% 
  filter(phon == 1) %>%
  drop_na() %>%
  group_by(type) %>%
  summarise(
    n = length(nLED), 
    total = nrow(.), 
    prop = length(nLED)/nrow(.)
  )

ggplot(token_phon_descriptives, aes(x = type, y = prop, fill = type)) + 
  geom_bar(stat = "identity")+
  labs(x = "Part of Speech") +
  labs(y = "Proportion of Phonological Shifts") +
  theme(
    title = element_text(colour="black", size = 20),
    axis.text.x = element_text(colour="grey20", size = 12),
    axis.text.y = element_text(colour="grey20", size = 12),  
    axis.title.x = element_text(colour="black", size = 14),
    axis.title.y = element_text(colour="black", size = 14),
    legend.position = "none"
  ) +
  geom_text(aes(label = round(prop, 2)), vjust = -0.5, size = 5) +
  coord_cartesian(ylim = c(0, 0.5)) +
  scale_fill_brewer(palette = "Accent")
```

![](C:/Users/g517364/Dropbox/GitHub/gruffalo_index/documents/02_plot-data_files/figure-markdown_github/token-phon-plot-1.png)

Here we can see that phonogical shifts affect function words (47.26%) and nouns (28.21%) more than any other word type.

#### How do Phonological Shifts Occur?

Next up, we'll look at how phonological shifts occur when they occur. That is, do the changes when translating a word from English to Scots typically involve changes to the vowels or some other type of change? We'll be quite granular here, focusing on changes like [monophthongization](https://en.wikipedia.org/wiki/Monophthongization) (e.g. English "house" to Scots "hoose") and final consonant drops (e.g. English "and" to Scots "an").

To do this in R, we make use of the brilliant functions for working with strings provided by `stringr` (prefixed by `str_`). We use this in combination with `dplyr::case_when` to clean up our names for plotting (removing underscores) by first detecting all strings with an underscore and + in the `shift_type` column, before replacing these characters with a space and & symbol. We finally make a broad category for plotting to make it a little easier to see when changes involve a vowel or not.

``` r
# must escape special characters when working with strings
replacements <- c(
  "\\_" = " ",
  "\\+" = " \\& "
)

token_shift_type <- token %>% 
  filter(phon == 1) %>%
  drop_na() %>%
  group_by(shift_type) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(
    shift_type = case_when(
    str_detect(shift_type, "\\_|\\+") ~ 
      str_replace_all(shift_type, replacements),
    TRUE ~ shift_type
    )
  ) %>% 
  # used for plotting differences in broad categories
  mutate(
    category = case_when(
      str_detect(
        shift_type, 
        "vowel shift|monophthongization|diphthongization"
      ) ~ "vowel_shift",
      TRUE ~ "other"
    )
  )
```

We can then plot this data with flipped axes for easier viewing like so.

``` r
token_shift_type %>%
  ggplot(aes(x = shift_type, y = n, fill = category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "How and how often do shifts occur across words?",
    x = "",
    y = "Count"
  ) +
  theme(
    plot.title = element_text(hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Accent") +
  annotate(
    "text", 
    x = 19.5, 
    y = 150, 
    label = "vowel-types", 
    size = 5, 
    fontface = "italic"
  ) +
  annotate(
    "text", 
    x = 2.5, 
    y = 125, 
    label = "other-types", 
    size = 5, 
    fontface = "italic"
  )
```

![](C:/Users/g517364/Dropbox/GitHub/gruffalo_index/documents/02_plot-data_files/figure-markdown_github/phon-shifts-how-plot-1.png)

We can see from this plot that shifts most often involve changes to the vowel. But, there's a lot of variability here, with many changes involving changes to both vowels and consonants. Let's reduce this down to two simple categories: how often is a phoneme substituted with something else, and how often is a consonant dropped entirely from the word?

``` r
c_drops <- token_shift_type %>% 
  filter(str_detect(shift_type, "consonant drop")) %>%
  summarise(n = sum(n)) %>%
  as.numeric()

subs <- token_shift_type %>% 
  filter(str_detect(
    shift_type, 
    "vowel shift|monophthongization|diphthongization"
  )) %>%
  summarise(n = sum(n)) %>%
  as.numeric()

token_shift_summary <- tibble(
  c_drops = c_drops,
  subs = subs
)
```

Approximately 0.24% of all phonological shifts involve dropping a consonant from the word, while 0.76% involve substution of a vowel.

### Exploring nLEDs

#### Densities by Category

How good was our categorisation into phonological or lexical shifts?

``` r
legend_title <- "Shift Category: "

token %>%
  ggplot() +
  geom_density(
    aes(
      x = nLED, 
      group = shift_category, 
      colour = shift_category, 
      fill = shift_category,
      y= ..scaled..
    ), 
    alpha = 0.5
  ) +
  theme(
    title = element_text(colour = "black", size = 20),
    axis.text.x = element_text(colour = "grey20", size = 12),
    axis.text.y = element_text(colour = "grey20", size = 12),  
    axis.title.x = element_text(colour = "black", size = 14),
    axis.title.y = element_text(colour = "black", size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "top",
    legend.background = element_rect(colour = "black"),
    legend.spacing.x = unit(0.1, "cm")
    ) +
  labs(
    x = "Normalised Levenshtein Edit Distance",
    y = "Scaled Density",
    fill = legend_title,
    colour = legend_title
  ) +
  scale_colour_viridis_d(option = "viridis", aesthetics = c("colour", "fill"))
```

![](C:/Users/g517364/Dropbox/GitHub/gruffalo_index/documents/02_plot-data_files/figure-markdown_github/token-density-plot-1.png)

We can see that, by and large, the nLEDs for words with a phonological shift are (unsurprisingly) unlike those that are unchanged, but they are also distinctly different those categorised as lexical shifts. Nicely, we can see that when we were unsure if a word involved a phonological or lexical shift, that the nLEDs for these words fell between the distributions of those in the phonological and lexical categories. That's quite reassuring!

#### Densities by Part of Speech

Finally, for these token frequencies, let's take a look at the nLEDs split by part of speech. This might give us some idea of how different parts of speech change. Are the separate parts-of-speech often involved in large changes to words when translating from English to Scots (associated with high nLEDs) or not?

``` r
token %>%
  drop_na() %>%
  ggplot(aes(x = nLED, y = type, fill = type)) +
  geom_density_ridges(scale = 3, alpha = 0.7, bandwidth = 0.0606) +
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  labs(x = "nLED", y = "Part of Speech") +
  scale_fill_brewer(palette = "Accent") +
  theme(legend.position = "none")
```

![](C:/Users/g517364/Dropbox/GitHub/gruffalo_index/documents/02_plot-data_files/figure-markdown_github/token-density-plot-POS-1.png)

Largely, across all parts of speech, any shifts are minimial. This indicates that no change or minor changes to phonology are most common across most parts of speech. We can also see some bimodal distributions and fat tails for distributions for certain categories. What's going on for nouns? Since this analysis includes many words (and all occurrences of them), it's likely that certain nouns rarely change (e.g. "time"), while others change very often (e.g. English "mouse" and Scots "moose"). This pattern might be masked by how frequently certain words occur. So, to get a better picture of how words change as a whole (without influences from word frequency), we might want to focus more on a *type frequency* analysis.

Type Frequencies
----------------

Here, we'll conduct a brief exploration into the type frequencies of words. This takes all unique words from the book in English, and looks at how often they change, and how they change. This just involves some simple counting.

``` r
head(type)
```

    ## # A tibble: 6 x 7
    ##   eng    count total_diff  none   lex  phon unsure
    ##   <chr>  <dbl>      <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 a        151          1     0     1     0      0
    ## 2 afraid     8          8     0     8     0      0
    ## 3 after      2          2     0     1     0      0
    ## 4 aha       12          8     0     4     0      0
    ## 5 ahead     10          7     0     2     0      0
    ## 6 all       13         13     0     0    13      0

We'll just focus here on how often words change by category, before we briefly look at how often words shift by part of speech.

### Exploring Shifts

#### Phonological Shifts

First, lets see how often phonological and lexical shifts occur across all words. This just looks at words with at least one occurrence of a phonological or lexical shift across all books and informants.

``` r
type_shifts <- type %>%
  summarise(
    n = n(),
    phon = sum(phon != 0),
    lex = sum(lex != 0)
  )
```

Phonological shifts occur 29.35% of the time across all books. When a shift occurs, this shift is phonological (rather than lexical) 38.4% of the time.

How does this map onto the different parts of speech that we explored above for the token frequency data?

To do this, we'll get only the unique occurrences of english words (and their associated part of speech, termed `type`). We'll then perform a join (using `dplyr::left_join`) on the type frequency data and these categories. This will add the parts of speech to the type data, joining by English word.

``` r
word_pos <- distinct(token, eng, type)

type_pos <- left_join(type, word_pos, by = "eng") %>%
  # filter(type != "proper noun") %>% # we don't want gruffalo making a big impact
  drop_na(type) %>%
  mutate(type = as.factor(type)) # enforces ordering for colours in plots
```

We can then do some operations on this data to see which words change (and how often), and which words never change.

``` r
changing_types <- type_pos %>% filter(total_diff != 0)
unchanging_types <- type_pos %>% filter(total_diff == 0)
```

We'll make some word cloud plots out of the type frequencies. This will show us all of the words from the corpus, with larger words occurring most often. We then colour the words according to their parts of speech (labelled in previous plots).

First off, let's look a words that never change from English to Scots.

``` r
ggplot(unchanging_types, aes(label = eng, size = count, colour = type)) +
    geom_text_wordcloud_area() +
    scale_size_area(max_size = 24) +
    scale_colour_brewer(palette = "Accent")
```

![](C:/Users/g517364/Dropbox/GitHub/gruffalo_index/documents/02_plot-data_files/figure-markdown_github/changing-types-wordcloud-1.png)

We can see that nouns change most often, with certain nouns (e.g. snake) changing very often when they're mentioned. Looking to the left of the plot, we can also see that many of the words that never change begin with a *t*. Whether or not this has any bearing on our findings is anyone's guess!

Next, we can look at the words that change at least once.

``` r
ggplot(changing_types, aes(label = eng, size = count, colour = type)) +
    geom_text_wordcloud_area() +
    scale_size_area(max_size = 24) +
    scale_colour_brewer(palette = "Accent")
```

![](C:/Users/g517364/Dropbox/GitHub/gruffalo_index/documents/02_plot-data_files/figure-markdown_github/unchanging-types-wordcloud-1.png)

We can see here that we have many more words that change at least once. What sticks out to me is the large number of nouns that change. We can see mouse, fox, child, house, wood, and owl, amongst others (or should that be moose, bairn, hoose, wid, and hoolet?)

Closing Remarks
===============

Taking all of these findings into consideration, you can see that Scots is a rich and diverse language. Even from such a simple exploration like this, it's apparent that Scots is very distinct to Standard British English. Hopefully, with books such as the Gruffalo and the Gruffalo's Child being translated into Scots, we can engage people with Scots and Scots literature, preserving such a distinct and interesting language for future generations.
