GROUP=scalakittens
ARTIFACT=core
VERSION=1.0.0
JAR=../../ScalaKittens/scala212/target/scala-2.12/scala-kittens-library-scala-2-12-2_2.12-1.0.0.jar
mvn install:install-file -DgroupId=$GROUP -DartifactId=$ARTIFACT -Dversion=$VERSION -Dfile=$JAR -Dpackaging=jar -DgeneratePom=true -DlocalRepositoryPath=.  -DcreateChecksum=true
