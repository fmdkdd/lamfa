BASE_CLASSPATH=base/
FACETS_CLASSPATH=facets/:/home/fmarch12/Utils/aspectj1.8/lib/aspectjrt.jar
AJC=~/Utils/aspectj1.8/bin/ajc
AJC_OPTS=-7 -cp ${FACETS_CLASSPATH} -d facets
JAVAC=javac
JAVAC_OPTS=-d base
JAVA=java
BASE_JAVA_OPTS=-cp ${BASE_CLASSPATH}
FACETS_JAVA_OPTS=-cp ${FACETS_CLASSPATH}

.PHONY: all run run-facets clean

all: base/Test.class facets/Test.class

run: base/Test.class
	${JAVA} ${BASE_JAVA_OPTS} Test

run-facets: facets/Test.class
	${JAVA} ${FACETS_JAVA_OPTS} Test

base/Test.class: Test.java
	${JAVAC} ${JAVAC_OPTS} Test.java

facets/Test.class: Test.java Facets.java
	${AJC} ${AJC_OPTS} Test.java Facets.java

clean:
	rm base/*.class facets/*.class
