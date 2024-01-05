package org.chiu.megalith;

import org.chiu.megalith.blog.dto.BLogEntityDto;
import org.chiu.megalith.infra.config.CustomRuntimeHints;
import org.springframework.aot.hint.annotation.RegisterReflectionForBinding;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ImportRuntimeHints;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;


@SpringBootApplication(proxyBeanMethods = false)
@EnableAsync
@EnableScheduling
@EnableMethodSecurity
@ImportRuntimeHints({ CustomRuntimeHints.class })
@RegisterReflectionForBinding(BLogEntityDto.class)
public class MegalithApplication {

	public static void main(String[] args) {
		SpringApplication.run(MegalithApplication.class, args);
	}

}
