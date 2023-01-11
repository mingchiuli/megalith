package com.chiu.megalith.common.config;

import org.springframework.amqp.core.Binding;
import org.springframework.amqp.core.BindingBuilder;
import org.springframework.amqp.core.DirectExchange;
import org.springframework.amqp.core.Queue;
import org.springframework.amqp.support.converter.Jackson2JsonMessageConverter;
import org.springframework.amqp.support.converter.MessageConverter;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @author mingchiuli
 * @create 2022-12-01 10:41 pm
 */
@Configuration
public class LogRabbitConfig {
    public static final String LOG_QUEUE = "log_queue";

    public static final String LOG_EXCHANGE = "log_exchange";

    public static final String LOG_BINDING_KEY = "log_exchange";

    @Bean
    public MessageConverter messageConverter(){
        return new Jackson2JsonMessageConverter();
    }

    //ES队列

    //LOG队列
    @Bean("LOG_QUEUE")
    public Queue logQueue() {
        return new Queue(LOG_QUEUE);
    }

    //LOG交换机
    @Bean("LOG_EXCHANGE")
    public DirectExchange logExchange() {
        return new DirectExchange(LOG_EXCHANGE);
    }

    @Bean
    public Binding LogBinding(@Qualifier("LOG_QUEUE") Queue logQueue, @Qualifier("LOG_EXCHANGE") DirectExchange logExchange) {
        return BindingBuilder.bind(logQueue).to(logExchange).with(LOG_BINDING_KEY);
    }
}