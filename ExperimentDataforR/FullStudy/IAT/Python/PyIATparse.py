import re, os, readline, csv
f=open("optiatcomma2", "r")
partline=re.compile("Participant") #get participant line, do actions
numsearch=re.compile("[0-9]+") #extract numbers from block string
datesearch=re.compile(r"\d+/\d+/\d+") #get dates from the date time lines
block124search=re.compile("Block [124]:") #get block 1,2,4
block35search=re.compile("Block [35]:") #get block 3 & 5
stimline=re.compile(r"\w+\W+\d+\W+\d{3,5}") #test line to ensure that it can be appended to previous block string
notnumsearch=re.compile("^[0-9]+") #match all not words
wordsearch=re.compile("[A-Za-z]+") #match words for extraction from block string
blocktimesearch=re.compile(",Block [1-5] Time:") # match block time line
fieldnames=("Participant", "Date", "Time", "Block", "Theirs", "Them", "Themselves", "Mine", "Me", "Myself", "Disimproving", "Failing", "Sad", "Worse", "Better", "Happy", "Improving", "Succeeding", "Correct", "BlockTime")
headers={"Participant":"Participant", "Date":"Date", "Time":"Time", "Block":"Block", "Theirs":"Theirs", "Them":"Them", "Themselves":"Themselves", "Mine":"Mine", "Me":"Me", "Myself":"Myself", "Disimproving":"Disimproving", "Failing":"Failing", "Sad":"Sad", "Worse":"Worse", "Better":"Better", "Happy":"Happy", "Improving":"Improving", "Succeeding":"Succeeding", "Correct":"Correct", "BlockTime":"BlockTime"}
fout=open("optiatres.csv", "w")
output=csv.DictWriter(fout, fieldnames=headers, restval="NA")
output.writerow(headers)
#optwriter=csv.writer(fout, delimiter=",", quotchar=" ", quoting=csv.QUOTE_MINIMAL)
def split_seq(seq,size): #used to split blocks and rejoin them properly
    """ Split up seq in pieces of size """ 
    return [seq[i:i+size] for i in range(0, len(seq), size)]

def flatten(l):
  out = []
  for item in l:
    if isinstance(item, (list, tuple)):
      out.extend(flatten(item))
    else:
      out.append(item)
  return out

for line in f:
    if  re.match(partline, line):
      line=line.rstrip()
      linesplit=re.split(",", line)
      partnum=linesplit[1]
          
    elif re.match(datesearch, line):
      dateTime=re.split(",", line)
      date=dateTime[0]
      time=dateTime[1]
      
    elif re.match(block124search, line):
      line=line.rstrip("\n")
      line2=f.next()
      line2=line2.rstrip("\n")
      linefull="%s%s" %(line, line2)
      line=re.split(",", linefull)
      block=line[0]
      line=line[1:]
      line=str(line)
      num=re.findall(numsearch,line)
      word=re.findall(wordsearch, line)
      numint=[int(x) for x in num]
      linenumsplit=split_seq(numint, 2)
      linewordsplit=split_seq(word,1)
      for i in range(len(linenumsplit)):
        linewordsplit[i].insert(1,linenumsplit[i])
      lineflat=flatten(linewordsplit)
      lineSplit=split_seq(lineflat, 3)
      linesort=sorted(lineSplit[:])
      from itertools import groupby
      result = []
      for key, gen in groupby(linesort, key=lambda x: x[0]):
        val = list(gen)
        svals = [y[2] for y in val]
        result.append([key, sum([y[1] for y in val]), sum(svals) / float(len(svals))])
      corr=[]
      for item in result:
        corr.append(item[1])
        corrsum=sum(corr)
        result2=[[x[0], x[2]] for x in result]
        result2dict=dict(result2)
      
    elif re.match(block35search, line):
      line=line.rstrip("\n")
      line2=f.next()
      line3=f.next()
      line4=f.next()
      line2=line2.rstrip()
      line3=line3.rstrip()
      line4=line4.rstrip()
      line="%s%s%s%s" %(line, line2, line3, line4)
      line=re.split(",", line)
      block=line[0]
      line=line[1:]
      line=str(line)
      num=re.findall(numsearch,line)
      word=re.findall(wordsearch, line)
      numint=[int(x) for x in num]
      linenumsplit=split_seq(numint, 2)
      linewordsplit=split_seq(word,1)
      for i in range(len(linenumsplit)):
        linewordsplit[i].insert(1,linenumsplit[i])
      lineflat=flatten(linewordsplit)
      lineSplit=split_seq(lineflat, 3)
      linesort=sorted(lineSplit[:])
      from itertools import groupby
      result = []
      for key, gen in groupby(sorted(linesort), key=lambda x: x[0]):
        val = list(gen)
        svals = [y[2] for y in val]
        result.append([key, sum([y[1] for y in val]), sum(svals) / float(len(svals))])
      
      corr=[]
      for item in result:
        corr.append(item[1])
      corrsum=sum(corr)
      result2=[[x[0], x[2]] for x in result]
      result2dict=dict(result2)
    elif re.match(blocktimesearch, line):
      print "Matched blocktime line"
      line=line.rstrip()
      linesplit=re.split(":", line)
      blocktime=linesplit[1]
      blocktime=blocktime.rstrip(",")
      print "Created Block Time"
      blockcompdict={"Participant":partnum, "Date": date, "Time":time, "Block":block, "Correct":corrsum, "BlockTime":blocktime}
      blockcompdict.update(result2dict)
      output.writerow(blockcompdict)
    elif len(line)==0:
      f.close()
      fout.close()
      print "All Done!"
                                
