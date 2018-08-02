# Evaluate the performance of NBA players
Use NBA play-by-play data from 13'-14' to 15'-16' seasons.

Find the starters of each quarter in each game.

Find the 10-people combination in every play.

Convert the play-by-play data to points of each play and other variables.

Modeling with BWLR with BGLR package in R. The weights are based on empirical estimation in diferent points differentials and game time.


### Scheme:
parse_player_xxxx -> Data_Conversion -> BWLR

The output of Data_Conversion would be 3 data of 3 seasons, respective, while the input of BWLR would be 1 bound data called all_play_corr.txt. You have to bind the data before running BWLR. 
