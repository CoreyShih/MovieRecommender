# MovieRecommender

This is the source code for a movie recommendation app based on the [book recommender app](https://github.com/pspachtholz/BookRecommender) by pspachtholz.

The app can be found [here](https://coreyshih.shinyapps.io/movierecommender/).

The app uses item-based collaborative filtering to provide recommendations and is trained on the [MovieLens 1M Dataset](https://grouplens.org/datasets/movielens/). For best results, provide varied ratings on as many movies as possible. Without sufficient varied ratings provided to the model, IBCF may return some or all `NA` values.

Additional thanks to [Feng Liang](https://stat.illinois.edu/directory/profile/liangf) for providing starter code and help with using recommenderlab.
