# github.observatory 1.0.4

## Improvments

* `collate-cran-desc` also stores [CRAN task views](https://cran.r-project.org/web/views/)

# github.observatory 1.0.3

## Improvments

* `collate-cran-desc` also stores:
  (1) title; 
  (2) description; 
  (3) version on CRAN; and 
  (4) latest update date on CRAN.


# github.observatory 1.0.2

## New Features

* `read_X` functions now cache tables in memory

## Improvments

* `Agent$recommend_users_to_user` with `method = random` draws recommendation by using the multinomial distribution. More R followers --> more chances of being recommended 

* `Agent$recommend_repos_to_user` with `method = random` draws recommendation by using the multinomial distribution. More Stargazers --> more chances of being recommended 

# github.observatory 1.0.1

## New Features

* Added `statistic` = "monthly downloads" to `Agent$query_package_stats`


