package com.chiu.megalith.coop.config;

import com.chiu.megalith.coop.mq.CoopMessageListener;
import lombok.RequiredArgsConstructor;
import org.springframework.amqp.core.*;
import org.springframework.amqp.rabbit.connection.ConnectionFactory;
import org.springframework.amqp.rabbit.listener.SimpleMessageListenerContainer;
import org.springframework.amqp.rabbit.listener.adapter.MessageListenerAdapter;
import org.springframework.amqp.support.converter.Jackson2JsonMessageConverter;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.UUID;

/**
 * @author mingchiuli
 * @create 2022-12-25 4:24 pm
 */
@Configuration(proxyBeanMethods = false)
@RequiredArgsConstructor
public class CoopRabbitConfig {

    public static String nodeMark;

    public static String WS_QUEUE = "coop.queue.";

    public static final String WS_TOPIC_EXCHANGE = "coop.topic.exchange";

    public static final String WS_BINDING_KEY = "coop.binding.";

    private final Jackson2JsonMessageConverter jsonMessageConverter;


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


    @Bean("MessageListenerAdapter")
    public MessageListenerAdapter coopMessageListener(CoopMessageListener coopMessageListener) {
        //	public static final String ORIGINAL_DEFAULT_LISTENER_METHOD = "handleMessage";
        return new MessageListenerAdapter(coopMessageListener);
    }

    @Bean("CoopMessageListenerContainer")
    public SimpleMessageListenerContainer coopMessageListenerContainer(ConnectionFactory connectionFactory,
                                                                       @Qualifier("MessageListenerAdapter") MessageListenerAdapter listenerAdapter,
                                                                       @Qualifier("COOP_QUEUE") Queue queue) {
        var container = new SimpleMessageListenerContainer();
        listenerAdapter.containerAckMode(AcknowledgeMode.AUTO);
        listenerAdapter.setMessageConverter(jsonMessageConverter);
        container.setConcurrency("5-10");
        container.setConnectionFactory(connectionFactory);
        container.setQueueNames(queue.getName());
        container.setMessageListener(listenerAdapter);
        return container;
    }

}
