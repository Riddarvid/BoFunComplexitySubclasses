
- Some packets are not on stackage
	- Add them as exttra dependencies
- The factors associated with False and True should be (1 - p) and p respectively. However, in computeMin, it seems that they have been mixed up.
	- Solution: switch the factors
- The constant threshold functions give the wrong sign: 
  thresholdConst False results in (0, 1) when it should result in (1, 0).
	- Solution: Tabulate over (/=) instead of (\==). A similar change is needed in setBit. See commit history for details.