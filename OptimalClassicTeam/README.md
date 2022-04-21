# Optimal AFL Fantasy Classic starting team (2022)
This program uses a linear-programming algorithm to find the optimal starting team in AFL Fantasy Classic. Usage requires file '2022 Player Prices and Positions.csv' which was scraped from the AFL Fantasy website.

# Current notes about the program:
- Dual-position players (DPPs) affect the program as they can count towards required counts of each position. For example, by including Tim English (RUC/FWD), the program will sometimes pick too many of another position.
To get around this, manually edit problematic players to a single position until a valid team structure is found (i.e. set Tim English as a RUC only on the positions file and run again).
- Does not include bench. For simplicity, the bench was set to 8 players all worth $190,000. If we have the perfect Classic team, we don't need bench cover.
- The captain is set to the highest scoring player on the team. This happens after the team is made, but hypotheically, it could be possible that there is a better team if captain is selected first.

# Program usage
Run all parts of the code to initialise functions and ensure you have the required packages. Then, type 'optimalteam(uptoround)' in the command window with the variable uptoround being the last round of data you want included.
For example, optimalteam(4) will show you the optimal starting team at the conclusion of Round 4. So using all data up to and including Round 4, it finds the best possible starting team.
