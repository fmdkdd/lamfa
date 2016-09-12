FACETS_CLASSPATH=out/:/home/fmdkdd/build/aspectj1.8/lib/aspectjrt.jar
AJC=/home/fmdkdd/build/aspectj1.8/bin/ajc
AJC_OPTS=-8 -cp ${FACETS_CLASSPATH} -d out
JAVAC=javac
JAVA=java
FACETS_JAVA_OPTS=-cp ${FACETS_CLASSPATH}

.PHONY: run clean

run: Test.class
	${JAVA} ${FACETS_JAVA_OPTS} Test

Test.class: Test.java
	${AJC} ${AJC_OPTS} Test.java

clean:
	rm *.class
