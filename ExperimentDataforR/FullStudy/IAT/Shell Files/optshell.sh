sed 's/,/\\/g' <optiat >optiatbs2 #replace commas with backslash
sed 's/\./\\/g' <optiatbs2 >optiatbs3 #replace . with \ 
sed 's/Time:.[0-9]*/&\\/g' <optiatbs3 >optiatbs4 #place \ after all block time
sed 's/Block [1-5]:/&\\/g' <optiatbs4 >optiatbs5 #place \ after the start of block stimuli
sed 's/[0-9].[0-9].:[0-9].:[0-9]./&\\/g' <optiatbs5 >optiatbs7 #put \ after the time field
sed 's/2011/&\\/g' <optiatbs7 >optiatbs8 # put \ after the date field
sed 's/Participant: [0-9][0-9][0-9][0-9]/&\\/g' <optiatbs8 >optiatbs9 # put \ after participant number
sed 's/Block 5 Time/\\&/g' <optiatbs9 >optiatbs12 #add \ between end of block 5 Time and last field of record
sed 's/Block 1 Time/\\&/g' <optiatbs12 >optiatbs13 #add \ between end of block 1 Time and last field of record
sed 's/Block 2 Time/\\&/g' <optiatbs13 >optiatbs14 #add \ between end of block 2 Time and last field of record
sed 's/Block 3 Time/\\&/g' <optiatbs14 >optiatbs15 #add \ between end of block 3 Time and last field of record
sed 's/Block 4 Time/\\&/g' <optiatbs15 >optiatbs16 #add \ between end of block 4 Time and last field of record
sed 's/\\/,/g' <optiatbs16 >optiatcomma #replace bs with commas
awk '{sub(/Participant:/, "Participant,"); print $0}' <optiatcomma >optiatcomma2 #insert comma after participant to enable part number to be identified as field.
#sed 's/flower essence/FlowerEssence/g' <optiatcomma2 >optiatcomma3