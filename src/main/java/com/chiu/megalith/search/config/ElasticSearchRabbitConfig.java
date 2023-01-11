package com.chiu.megalith.search.config;

import org.springframework.amqp.core.Binding;
import org.springframework.amqp.core.BindingBuilder;
import org.springframework.amqp.core.DirectExchange;
import org.springframework.amqp.core.Queue;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @author mingchiuli
 * @create 2022-12-25 4:13 pm
 */
@Configuration
public class ElasticSearchRabbitConfig {

    public static final String ES_QUEUE = "es_queue";

    public static final String ES_EXCHANGE = "es_exchange";

    public static final String ES_BINDING_KEY = "es_exchange";

    @Bean("ES_QUEUE")
    public Queue esQueue() {
        return new Queue(ES_QUEUE);
    }

    //ES交换机
    @Bean("ES_EXCHANGE")
    public DirectExchange esExchange() {
        return new DirectExchange(ES_EXCHANGE);
    }

    //绑定ES队列和ES交换机
    @Bean
    public Binding esBinding(@Qualifier("ES_QUEUE") Queue esQueue, @Qualifier("ES_EXCHANGE") DirectExchange esExchange) {
        return BindingBuilder.bind(esQueue).to(esExchange).with(ES_BINDING_KEY);
    }
}
