import org.springframework.boot.gradle.tasks.bundling.BootBuildImage

plugins {
	java
	id("org.springframework.boot") version "3.2.5"
	id("io.spring.dependency-management") version "1.1.4"
	id("org.graalvm.buildtools.native") version "0.10.1"
}

group = "com.chiu"
version = "0.0.1"
java.sourceCompatibility = JavaVersion.VERSION_21

configurations {
	compileOnly {
		extendsFrom(configurations.annotationProcessor.get())
	}
}

repositories {
	mavenCentral()
}

dependencies {
	implementation("org.springframework.boot:spring-boot-starter-actuator")
	implementation("org.springframework.boot:spring-boot-starter-amqp")
	implementation("org.springframework.boot:spring-boot-starter-data-elasticsearch")
	implementation("org.springframework.boot:spring-boot-starter-data-jpa")
	implementation("org.springframework.boot:spring-boot-starter-data-redis")
	implementation("org.springframework.boot:spring-boot-starter-security")
	implementation("org.springframework.boot:spring-boot-starter-validation")
	implementation("org.springframework.boot:spring-boot-starter-web")
	implementation("org.springframework.boot:spring-boot-starter-websocket")
	implementation("org.springframework.boot:spring-boot-starter-mail")
	implementation("org.springframework.security:spring-security-messaging")
	implementation("com.auth0:java-jwt:4.4.0")
	implementation("org.redisson:redisson-spring-boot-starter:3.27.0")
	implementation("com.github.ben-manes.caffeine:caffeine:3.1.8")
	runtimeOnly("org.mariadb.jdbc:mariadb-java-client")
	compileOnly("org.projectlombok:lombok")
	annotationProcessor("org.springframework.boot:spring-boot-configuration-processor")
	annotationProcessor("org.projectlombok:lombok")
	testImplementation("org.springframework.boot:spring-boot-starter-test")
	testImplementation("org.springframework.amqp:spring-rabbit-test")
	testImplementation("org.springframework.security:spring-security-test")
}

tasks.withType<Test> {
	useJUnitPlatform()
}

tasks.named<BootBuildImage>("bootBuildImage") {
	docker {
		publish.set(true)
		publishRegistry {
			url.set("docker.io")
			val un = System.getenv("DOCKER_USERNAME")
			val tn = System.getenv("DOCKER_PWD")
		
		
                        token.set(tn)
		}
	}
}

