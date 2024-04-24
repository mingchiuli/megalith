package org.chiu.megalith.blog.config;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.blog.cache.mq.CacheBlogEvictMessageListener;
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
public class CacheBlogEvictRabbitConfig {

    public String evictNodeMark;

    public static String CACHE_BLOG_EVICT_QUEUE = "cache.blog.evict.queue.";

    public static final String CACHE_BLOG_EVICT_FANOUT_EXCHANGE = "cache.blog.evict.fanout.exchange";

    private final Jackson2JsonMessageConverter jsonMessageConverter;

    @Qualifier("mqExecutor")
    private final TaskExecutor executor;

    @Bean("CACHE_BLOG_EVICT_FANOUT_QUEUE")
    Queue evictQueue() {
        evictNodeMark = UUID.randomUUID().toString();
        CACHE_BLOG_EVICT_QUEUE += evictNodeMark;
        return new Queue(CACHE_BLOG_EVICT_QUEUE, false, true, true);
    }

    @Bean("CACHE_BLOG_EVICT_FANOUT_EXCHANGE")
    FanoutExchange evictExchange() {
        return new FanoutExchange(CACHE_BLOG_EVICT_FANOUT_EXCHANGE, true, false);
    }

    @Bean("CACHE_BLOG_EVICT_FANOUT_BINDING")
    Binding evictBinding(@Qualifier("CACHE_BLOG_EVICT_FANOUT_QUEUE") Queue cacheQueue,
                         @Qualifier("CACHE_BLOG_EVICT_FANOUT_EXCHANGE") FanoutExchange cacheExchange) {
        return BindingBuilder
                .bind(cacheQueue)
                .to(cacheExchange);
    }

    @Bean("cacheBlogEvictMessageListenerAdapter")
    MessageListenerAdapter coopMessageListener(CacheBlogEvictMessageListener cacheBlogEvictMessageListener) {
        //	public static final String ORIGINAL_DEFAULT_LISTENER_METHOD = "handleMessage";
        return new MessageListenerAdapter(cacheBlogEvictMessageListener);
    }

    @Bean("cacheEvictMessageListenerContainer")
    SimpleMessageListenerContainer cacheEvictMessageListenerContainer(ConnectionFactory connectionFactory,
                                                                      @Qualifier("cacheBlogEvictMessageListenerAdapter") MessageListenerAdapter listenerAdapter,
                                                                      @Qualifier("CACHE_BLOG_EVICT_FANOUT_QUEUE") Queue queue) {
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
