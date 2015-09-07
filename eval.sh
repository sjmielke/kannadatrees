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
			java $MEM -jar /usr/share/java/maltparser/maltparser-1.8.1.jar -c test -i $1_train.conll -m learn -a $TRAINALGO
			java $MEM -jar /usr/share/java/maltparser/maltparser-1.8.1.jar -c test -i $1_test.conll -o out.conll -m parse
			java $MEM -jar ../malteval-dist-20141005/lib/MaltEval.jar -g $1_test.conll -s out.conll -v 0
			cd ../code
			;;
		optikannada)
			cd ../data
			COMMONOPTI="-c test -f ../MaltOptimizer-1.0.3/finalOptionsFile.xml -F ../MaltOptimizer-1.0.3/addStackFEATS0.xml"
			java $MEM -jar /usr/share/java/maltparser/maltparser-1.8.1.jar $COMMONOPTI -i $1_train.conll -m learn
			java $MEM -jar /usr/share/java/maltparser/maltparser-1.8.1.jar $COMMONOPTI -i $1_test.conll -o out.conll -m parse
			java $MEM -jar ../malteval-dist-20141005/lib/MaltEval.jar -g $1_test.conll -s out.conll -v 0
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
			java se.vxu.msi.malteval.MaltEvalConsole -g ../../../data/$1_test.conll -s ../../../data/out.conll -v 1
			cd ../../../code
			;;
		*)
			# Yup, this is also called when we specify the language in $1
			# Might fix it, might also not.
			TRAINALGO=$arg
	esac
done
