awk '/Participant: [0-9]+ Test: Treatment Credibility IAT/, /Block 5 Time:/ {print $0  >>"tcqiat2" }' IATData120211.txt #these  worked from the command line, woo hoo!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
awk '/Participant: [0-9]+ Test: Optimism IAT/, /Block 5 Time:/ {print $0  >>"optiat2" }' IATData120211.txt
sed 's/,/\\/g' <opttestbs >opttestbs2 #replace commas with backslash
sed 's/:/\\/g' <opttestbs2 >opttestbs3 #substitute : for \ Messed up times, don't use.
sed 's/\./\\/g' <opttestbs3 >opttestbs5 #replace . with \ 
sed 's/Time:.[0-9]*/&\\/g' <opttestbs2 >opttestbs6 #place \ after all block time
sed 's/Block [1-5]:/&\\/g' <opttestbs7 >opttestbs8 #place \ after the start of block stimuli
sed 's/[0-9].[0-9].:[0-9].:[0-9]./&\\/g' <opttestbs8 >opttestbs9 #put \ after the time field
sed 's/2011/&\\/g' <opttestbs9 >opttestbs10 # put \ after the date field
sed 's/Participant: [0-9]{4,9}/&\\/g' <opttestbs10 >opttestbs11 # put \ after participant number
sed 's/Block [1-5] Time/&\\/g' <opttestbs12 >opttestbs13
sed 's/Block [1-5] Time/&\\/g' <opttestbs13 >opttestbs14
sed 's/Block 5 Time/\\&/g' <opttestbs12 >opttestbs13 #add \ between end of block 5 Time and last field of record
sed 's/Block 1 Time/\\&/g' <opttestbs13 >opttestbs14 #add \ between end of block 1 Time and last field of record
sed 's/Block 2 Time/\\&/g' <opttestbs14 >opttestbs15 #add \ between end of block 2 Time and last field of record
sed 's/Block 3 Time/\\&/g' <opttestbs15 >opttestbs16 #add \ between end of block 3 Time and last field of record
sed 's/Block 4 Time/\\&/g' <opttestbs16 >opttestbs17 #add \ between end of block 4 Time and last field of record
sed 's/\\/,/g' <opttestbs17 >opttestcomma #replace bs with commas
awk '{sub(/Participant:/, "Participant,"); print $0}' opttestcomma >opttestcomma2 #insert comma after participant to enable part number to be identified as field.
sed 's/flower essence/FlowerEssence/g' <tcqiatcomma3 >tcqiatcomma4
