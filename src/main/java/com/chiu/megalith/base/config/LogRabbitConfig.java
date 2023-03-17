package com.chiu.megalith.base.config;

import org.springframework.amqp.core.Binding;
import org.springframework.amqp.core.BindingBuilder;
import org.springframework.amqp.core.DirectExchange;
import org.springframework.amqp.core.Queue;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @author mingchiuli
 * @create 2022-12-01 10:41 pm
 */
@Configuration(proxyBeanMethods = false)
public class LogRabbitConfig {
    public static final String LOG_QUEUE = "log.queue";

    public static final String LOG_EXCHANGE = "log.direct.exchange";

    public static final String LOG_BINDING_KEY = "log.binding";

    //LOG队列
    @Bean("LOG_QUEUE")
    public Queue queue() {
        return new Queue(LOG_QUEUE);
    }

    //LOG交换机
    @Bean("LOG_EXCHANGE")
    public DirectExchange exchange() {
        return new DirectExchange(LOG_EXCHANGE);
    }

    @Bean("LOG_BINDING")
    public Binding binding(@Qualifier("LOG_QUEUE") Queue logQueue,
                           @Qualifier("LOG_EXCHANGE") DirectExchange logExchange) {
        return BindingBuilder.
                bind(logQueue).
                to(logExchange).
                with(LOG_BINDING_KEY);
    }
}
