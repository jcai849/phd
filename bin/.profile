export HADOOP_HOME=/usr/hadoop
export HADOOP_CONF_DIR=${HADOOP_HOME}/etc/hadoop
export SPARK_HOME=/shared/spark
export JAVA_HOME=/shared/jdk
export PATH=${JAVA_HOME}/bin:${HADOOP_HOME}/bin:${HADOOP_HOME}/sbin:$PATH:${SPARK_HOME}/bin:${HOME}/bin
