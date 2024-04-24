package org.chiu.megalith.blog.config;

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
public class EvictCacheRabbitConfig {

    public static final String CACHE_BLOG_EVICT_QUEUE = "cache.blog.evict.queue";

    public static final String CACHE_BLOG_EVICT_EXCHANGE = "cache.blog.direct.exchange";

    public static final String CACHE_BLOG_EVICT_BINDING_KEY = "cache.blog.evict.binding";

    @Bean("CACHE_BLOG_EVICT_QUEUE")
    Queue queue() {
        return new Queue(CACHE_BLOG_EVICT_QUEUE, true, false, false);
    }

    @Bean("CACHE_BLOG_EVICT_EXCHANGE")
    DirectExchange exchange() {
        return new DirectExchange(CACHE_BLOG_EVICT_EXCHANGE, true, false);
    }

    @Bean("CACHE_BLOG_EVICT_BINDING")
    Binding binding(@Qualifier("CACHE_BLOG_EVICT_QUEUE") Queue esQueue,
                    @Qualifier("CACHE_BLOG_EVICT_EXCHANGE") DirectExchange esExchange) {
        return BindingBuilder
                .bind(esQueue)
                .to(esExchange)
                .with(CACHE_BLOG_EVICT_BINDING_KEY);
    }
}
