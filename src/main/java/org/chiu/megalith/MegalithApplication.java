package org.chiu.megalith;

import org.chiu.megalith.infra.config.CustomRuntimeHints;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ImportRuntimeHints;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;


@SpringBootApplication(proxyBeanMethods = false)
@EnableAsync
@EnableScheduling
@EnableMethodSecurity
@EnableJpaAuditing
@ImportRuntimeHints({ CustomRuntimeHints.class })
public class MegalithApplication {

	public static void main(String[] args) {
				SpringApplication.run(MegalithApplication.class, args);
		}

}
