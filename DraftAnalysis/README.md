# AFL Fantasy Draft Analysis
Want to assess your own AFL Fantasy drafting skills this year? This program can show the best picks for any round during the season. 

## Usage
* Ensure you have the three required packages installed (lines 1-3). You can do this using install.packages("fitzRoy"), for example.
* Run each line to import the data. 
  	* Line 4 gets the latest data from the AFL
  	* Line 5 imports the ADP data I took just before Round 1.
  	* Line 8 has team colours, credit to @crow_data_sci
  	* And, line 29 will initialise the program ready for use.
* To run the program, you can now type 'draft_analysis(uptoround)' in the console, replacing 'uptoround' with your round of choice.
  * For example, 'draft_analysis(5)' will show the best picks using data up to Round 5 of the 2022 season.

## Output
The program will display a plot of all players, with their average draft position on the x-axis and their total points so far on the y-axis. The grey line is a local regression line which, simply, is the running average player. If players are above this line, they've had a better than predicted season so far. And of course, the opposite applies to those under the line. The plot helps to visualise how well we drafted as AFL Fantasy coaches this year.
A table is also displayed with each pick ordered by value.
