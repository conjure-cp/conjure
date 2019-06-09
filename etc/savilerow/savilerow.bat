

# get savilerow installation directory
DIR="$( cd "$( dirname "$0" )" && pwd )"

java -ea -XX:ParallelGCThreads=1 -Xmx8G -jar "$DIR/savilerow.jar" %*
