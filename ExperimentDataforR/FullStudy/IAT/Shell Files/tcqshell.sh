sed 's/,/\\/g' <tcqiat >tcqiatbs2 #replace commas with backslash
sed 's/\./\\/g' <tcqiatbs2 >tcqiatbs3 #replace . with \ 
sed 's/Time:.[0-9]*/&\\/g' <tcqiatbs3 >tcqiatbs4 #place \ after all block time
sed 's/Block [1-5]:/&\\/g' <tcqiatbs4 >tcqiatbs5 #place \ after the start of block stimuli
sed 's/[0-9].[0-9].:[0-9].:[0-9]./&\\/g' <tcqiatbs5 >tcqiatbs7 #put \ after the time field
sed 's/2011/&\\/g' <tcqiatbs7 >tcqiatbs8 # put \ after the date field
sed 's/Participant: [0-9][0-9][0-9][0-9]/&\\/g' <tcqiatbs8 >tcqiatbs9 # put \ after participant number
sed 's/Block 5 Time/\\&/g' <tcqiatbs9 >tcqiatbs12 #add \ between end of block 5 Time and last field of record
sed 's/Block 1 Time/\\&/g' <tcqiatbs12 >tcqiatbs13 #add \ between end of block 1 Time and last field of record
sed 's/Block 2 Time/\\&/g' <tcqiatbs13 >tcqiatbs14 #add \ between end of block 2 Time and last field of record
sed 's/Block 3 Time/\\&/g' <tcqiatbs14 >tcqiatbs15 #add \ between end of block 3 Time and last field of record
sed 's/Block 4 Time/\\&/g' <tcqiatbs15 >tcqiatbs16 #add \ between end of block 4 Time and last field of record
sed 's/\\/,/g' <tcqiatbs16 >tcqiatcomma #replace bs with commas
awk '{sub(/Participant:/, "Participant,"); print $0}' <tcqiatcomma >tcqiatcomma2 #insert comma after participant to enable part number to be identified as field.
sed 's/flower essence/FlowerEssence/g' <tcqiatcomma2 >tcqiatcomma3