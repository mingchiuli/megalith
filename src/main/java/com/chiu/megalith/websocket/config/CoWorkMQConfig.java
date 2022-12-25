package com.chiu.megalith.websocket.config;

import org.springframework.amqp.core.Binding;
import org.springframework.amqp.core.BindingBuilder;
import org.springframework.amqp.core.Queue;
import org.springframework.amqp.core.TopicExchange;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.UUID;

/**
 * @author mingchiuli
 * @create 2022-12-25 4:24 pm
 */
@Configuration
public class CoWorkMQConfig {

    public static String serverMark;

    public static String WS_QUEUE = "ws_queue_";

    public static final String WS_TOPIC_EXCHANGE = "ws_topic_exchange";

    public static final String WS_BINDING_KEY = "ws_exchange_";

    @Bean("WS_QUEUE")
    public Queue wsQueue() {
        serverMark = UUID.randomUUID().toString();
        WS_QUEUE += serverMark;
        return new Queue(WS_QUEUE);
    }

    @Bean("WS_TOPIC_EXCHANGE")
    public TopicExchange wsExchange() {
        return new TopicExchange(WS_TOPIC_EXCHANGE);
    }

    @Bean
    public Binding wsTopicBinding(@Qualifier("WS_QUEUE") Queue wsQueue, @Qualifier("WS_TOPIC_EXCHANGE") TopicExchange wsExchange) {
        return BindingBuilder.bind(wsQueue).to(wsExchange).with(WS_BINDING_KEY + serverMark);
    }

}
