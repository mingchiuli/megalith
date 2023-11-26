package org.chiu.megalith.infra.config;

import lombok.RequiredArgsConstructor;

import org.chiu.megalith.infra.listener.CacheMessageListener;
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
public class CacheRabbitConfig {

    public String nodeMark;

    public static String CACHE_QUEUE = "cache.queue.";

    public static final String CACHE_FANOUT_EXCHANGE = "cache.fanout.exchange";

    private final Jackson2JsonMessageConverter jsonMessageConverter;

    @Qualifier("mqExecutor")
    private final TaskExecutor executor;

    @Bean("CACHE_QUEUE")
    Queue queue() {
        nodeMark = UUID.randomUUID().toString();
        CACHE_QUEUE += nodeMark;
        return new Queue(CACHE_QUEUE, false, true, true);
    }

    @Bean("CACHE_FANOUT_EXCHANGE")
    FanoutExchange exchange() {
        return new FanoutExchange(CACHE_FANOUT_EXCHANGE, true, false);
    }

    @Bean("CACHE_BINDING")
    Binding binding(@Qualifier("CACHE_QUEUE") Queue cacheQueue,
                    @Qualifier("CACHE_FANOUT_EXCHANGE") FanoutExchange cacheExchange) {
        return BindingBuilder
                .bind(cacheQueue)
                .to(cacheExchange);
    }

    @Bean("cacheMessageListenerAdapter")
    MessageListenerAdapter coopMessageListener(CacheMessageListener cacheMessageListener) {
        //	public static final String ORIGINAL_DEFAULT_LISTENER_METHOD = "handleMessage";
        return new MessageListenerAdapter(cacheMessageListener);
    }

    @Bean("cacheMessageListenerContainer")
    SimpleMessageListenerContainer coopMessageListenerContainer(ConnectionFactory connectionFactory,
                                                                @Qualifier("cacheMessageListenerAdapter") MessageListenerAdapter listenerAdapter,
                                                                @Qualifier("CACHE_QUEUE") Queue queue) {
        var container = new SimpleMessageListenerContainer();
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
