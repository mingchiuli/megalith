package org.chiu.megalith.infra.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.concurrent.*;

/**
 * @author mingchiuli
 * @create 2022-04-26 10:06 PM
 */
@Configuration(proxyBeanMethods = false)
public class ThreadPoolConfig {

    @Bean("taskExecutor")
    ExecutorService executorService() {
        return Executors.newVirtualThreadPerTaskExecutor();
    }
}
