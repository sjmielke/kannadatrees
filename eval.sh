#! /bin/bash

# Immediately exit if something fails.
set -e

MEM="-Xmx2400m"

for arg in "$@"
do
	case $arg in
		compile)
			ghc -main-is Parse$1TB Parse$1TB -o parse -Wall -O2
			./parse
			rm parse Parse$1TB.{hi,o}
			;;
		train)
			cd ../data
			java $MEM -jar /usr/share/java/maltparser/maltparser-1.8.1.jar -c test -i $1_train.conll -m learn $BONUSFLAG
			java $MEM -jar /usr/share/java/maltparser/maltparser-1.8.1.jar -c test -i $1_test.conll -o $1_parsed.conll -m parse
			java $MEM -jar ../malteval-dist-20141005/lib/MaltEval.jar -g $1_gold.conll -s $1_parsed.conll -v 0
			cd ../code
			;;
		optitrain)
			cd ../MaltOptimizer-1.0.3
			COMMONCMD="java -jar MaltOptimizer.jar"
			COMMONFLAGS="-m /usr/share/java/maltparser/maltparser-1.8.1.jar -c ../data/$1_train.conll -v cv"
			$COMMONCMD -p 1 $COMMONFLAGS &&
			$COMMONCMD -p 2 $COMMONFLAGS &&
			$COMMONCMD -p 3 $COMMONFLAGS
			cp finalOptionsFile.xml ../data/$1_finalOptionsFile.xml
			echo "REMEMBER TO COPY THE SPECIFIED FEATS FILE TO 'data/$1_FEATS.xml'!"
			cd ../code
			;;
		optitest)
			cd ../data
			COMMONCMD="java $MEM -jar /usr/share/java/maltparser/maltparser-1.8.1.jar -c test -f $1_finalOptionsFile.xml -F $1_FEATS.xml"
			$COMMONCMD -i $1_train.conll -m learn $BONUSFLAG
			$COMMONCMD -i $1_test.conll -o $1_parsed.conll -m parse
			java $MEM -jar ../malteval-dist-20141005/lib/MaltEval.jar -g $1_gold.conll -s $1_parsed.conll -v 0
			cd ../code
			;;
		treeview)
			cd ../malteval-src/ant/bin
			# If you have the same problem as me ("ClassCastException" when
			# using unicode letters in the TreeViewer), replace the content
			# of the inner for loop starting at
			# se/vxu/msi/malteval/treeviewer/MaltTreeViewGui:275 with:
			# 
			# boolean isok = true;
			# try {
			#     isok = usableFonts.get(j).canDisplayUpTo(treebank.getSentence(i).toString()) == -1;
			# } catch (ClassCastException e) {
			#     System.out.println("Failing at " + usableFonts.get(j).toString());
			#     isok = false;
			# }
			# if (!isok) {
			#     usableFonts.remove(j);
			#     j--;
			# }
			java se.vxu.msi.malteval.MaltEvalConsole -g ../../../data/$1_gold.conll -s ../../../data/$1_parsed.conll -v 1
			cd ../../../code
			;;
		Kannada|Hamburg)
			:
			;;
		*)
			BONUSFLAG=$arg
	esac
done
