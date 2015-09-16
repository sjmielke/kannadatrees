#! /bin/bash

# Immediately exit if something fails.
set -e

MEM="-Xmx2200m"

for arg in "$@"
do
	case $arg in
		compile)
			ghc -main-is Parse$1TB Parse$1TB -o parse -Wall -O2
			./parse
			rm parse *.{hi,o}
			;;
		evaluate)
			cd ../data
			java $MEM -jar ../malteval-dist-20141005/lib/MaltEval.jar -g $1/sentences/gold*.conll -s $1/sentences/parsed*.conll -v 0 $BONUSFLAG
			cd ../code
			;;
		optimize)
			cd ../MaltOptimizer-1.0.3
			COMMONCMD="java -jar MaltOptimizer.jar"
			# The python-aliasing is just for me, use at your own discretion.
			if [ ! -d /tmp/bin ]; then
				mkdir /tmp/bin; ln -s /usr/bin/python2 /tmp/bin/python
			fi
			export PATH=/tmp/bin:$PATH
			COMMONFLAGS="-m /usr/share/java/maltparser/maltparser-1.8.1.jar -c ../data/$1/sentences/full.conll -v cv"
			$COMMONCMD -p 1 $COMMONFLAGS &&
			$COMMONCMD -p 2 $COMMONFLAGS &&
			$COMMONCMD -p 3 $COMMONFLAGS
			cp finalOptionsFile.xml ../data/$1/finalOptionsFile.xml
			echo "REMEMBER TO COPY THE SPECIFIED FEATS FILE TO 'data/$1/FEATS.xml'!"
			cd ../code
			;;
		train)
			cd ../data
			COMMONCMD="java $MEM -jar /usr/share/java/maltparser/maltparser-1.8.1.jar -f $1/finalOptionsFile.xml -F $1/FEATS.xml"
			for i in {1..5} # hardcoded in converter
			do
				echo;echo
				echo "### Now training and testing fold $i"
				echo
				$COMMONCMD -i $1/sentences/train$i.conll -m learn $BONUSFLAG
				$COMMONCMD -i $1/sentences/test$i.conll -o $1/sentences/parsed$i.conll -m parse
			done
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
			java se.vxu.msi.malteval.MaltEvalConsole -g ../../../data/$1/sentences/gold1.conll -s ../../../data/$1/sentences/parsed1.conll -v 1
			cd ../../../code
			;;
		Kannada|Hamburg)
			:
			;;
		*)
			BONUSFLAG=$arg
	esac
done
