package org.chiu.megalith.manage.config;

import lombok.RequiredArgsConstructor;

import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.manage.listener.cache.mq.CacheEvictMessageListener;
import org.springframework.amqp.core.*;
import org.springframework.amqp.rabbit.connection.ConnectionFactory;
import org.springframework.amqp.rabbit.listener.SimpleMessageListenerContainer;
import org.springframework.amqp.rabbit.listener.adapter.MessageListenerAdapter;
import org.springframework.amqp.support.converter.Jackson2JsonMessageConverter;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;

import java.util.UUID;

@Configuration(proxyBeanMethods = false)
@RequiredArgsConstructor
public class CacheEvictRabbitConfig {

    public String evictNodeMark;

    public static String CACHE_EVICT_QUEUE = "cache.evict.queue.";

    public static final String CACHE_EVICT_FANOUT_EXCHANGE = Const.CACHE_EVICT_FANOUT_EXCHANGE.getInfo();

    private final Jackson2JsonMessageConverter jsonMessageConverter;

    @Qualifier("mqExecutor")
    private final TaskExecutor executor;

    @Bean("CACHE_EVICT_QUEUE")
    Queue evictQueue() {
        evictNodeMark = UUID.randomUUID().toString();
        CACHE_EVICT_QUEUE += evictNodeMark;
        return new Queue(CACHE_EVICT_QUEUE, false, true, true);
    }

    @Bean("CACHE_EVICT_FANOUT_EXCHANGE")
    FanoutExchange evictExchange() {
        return new FanoutExchange(CACHE_EVICT_FANOUT_EXCHANGE, true, false);
    }

    @Bean("CACHE_EVICT_BINDING")
    Binding evictBinding(@Qualifier("CACHE_EVICT_QUEUE") Queue cacheQueue,
                         @Qualifier("CACHE_EVICT_FANOUT_EXCHANGE") FanoutExchange cacheExchange) {
        return BindingBuilder
                .bind(cacheQueue)
                .to(cacheExchange);
    }

    @Bean("cacheEvictMessageListenerAdapter")
    MessageListenerAdapter coopMessageListener(CacheEvictMessageListener cacheEvictMessageListener) {
        //	public static final String ORIGINAL_DEFAULT_LISTENER_METHOD = "handleMessage";
        return new MessageListenerAdapter(cacheEvictMessageListener);
    }

    @Bean("cacheEvictMessageListenerContainer")
    SimpleMessageListenerContainer cacheEvictMessageListenerContainer(ConnectionFactory connectionFactory,
                                                                @Qualifier("cacheEvictMessageListenerAdapter") MessageListenerAdapter listenerAdapter,
                                                                @Qualifier("CACHE_EVICT_QUEUE") Queue queue) {
        var container = new SimpleMessageListenerContainer();
        //框架处理了
        listenerAdapter.containerAckMode(AcknowledgeMode.MANUAL);
        listenerAdapter.setMessageConverter(jsonMessageConverter);
        container.setConcurrency("5");
        container.setConnectionFactory(connectionFactory);
        container.setQueueNames(queue.getName());
        container.setMessageListener(listenerAdapter);
        container.setTaskExecutor(executor);
        return container;
    }
}
