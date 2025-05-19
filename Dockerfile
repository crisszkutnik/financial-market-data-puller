FROM amazoncorretto:21-alpine-jdk

WORKDIR /app

COPY target/scala-3.3.6/application.jar /app

EXPOSE 8080
EXPOSE 9400

CMD ["java", "-jar", "application.jar"]