package com.chiu.megalith.coop.config;

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
@Configuration(proxyBeanMethods = false)
public class CoopRabbitConfig {

    public static String nodeMark;

    public static String WS_QUEUE = "coop.queue.";

    public static final String WS_TOPIC_EXCHANGE = "coop.topic.exchange";

    public static final String WS_BINDING_KEY = "coop.binding.";

    @Bean("COOP_QUEUE")
    public Queue queue() {
        nodeMark = UUID.randomUUID().toString();
        WS_QUEUE += nodeMark;
        return new Queue(WS_QUEUE, true, false, true);
    }

    @Bean("COOP_TOPIC_EXCHANGE")
    public TopicExchange exchange() {
        return new TopicExchange(WS_TOPIC_EXCHANGE);
    }

    @Bean("COOP_BINDING")
    public Binding binding(@Qualifier("COOP_QUEUE") Queue wsQueue,
                           @Qualifier("COOP_TOPIC_EXCHANGE") TopicExchange wsExchange) {
        return BindingBuilder
                .bind(wsQueue)
                .to(wsExchange)
                .with(WS_BINDING_KEY + nodeMark);
    }

}
