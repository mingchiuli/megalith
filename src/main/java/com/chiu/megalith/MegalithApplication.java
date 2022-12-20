package com.chiu.megalith;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;


@SpringBootApplication(proxyBeanMethods = false)
@EnableAsync
@EnableScheduling
public class MegalithApplication {

	public static void main(String[] args) {
		SpringApplication.run(MegalithApplication.class, args);
	}

}
