package org.chiu.megalith.search.config;

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
@Configuration(proxyBeanMethods = false)
public class ElasticSearchRabbitConfig {

    public static final String ES_QUEUE = "es.queue";

    public static final String ES_EXCHANGE = "es.direct.exchange";

    public static final String ES_BINDING_KEY = "es.binding";

    @Bean("ES_QUEUE")
    Queue queue() {
        return new Queue(ES_QUEUE);
    }

    //ES交换机
    @Bean("ES_EXCHANGE")
    DirectExchange exchange() {
        return new DirectExchange(ES_EXCHANGE);
    }

    //绑定ES队列和ES交换机
    @Bean("ES_BINDING")
    Binding binding(@Qualifier("ES_QUEUE") Queue esQueue,
                    @Qualifier("ES_EXCHANGE") DirectExchange esExchange) {
        return BindingBuilder
                .bind(esQueue)
                .to(esExchange)
                .with(ES_BINDING_KEY);
    }
}
