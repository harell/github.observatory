# github.observatory 1.0.2

## New Features

* `read_X` functions now cache tables in memory


# github.observatory 1.0.1

## New Features

* Added `statistic` = "monthly downloads" to `Agent$query_package_stats`

## Improvments

* `Agent$recommend_users_to_user` with `method = random` draws recommendation by using the multinomial distribution. More R followers --> more chances of being recommended 

* `Agent$recommend_repos_to_user` with `method = random` draws recommendation by using the multinomial distribution. More Stargazers --> more chances of being recommended 
