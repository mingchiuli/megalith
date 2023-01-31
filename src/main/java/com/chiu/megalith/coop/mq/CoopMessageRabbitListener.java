package com.chiu.megalith.coop.mq;

import com.chiu.megalith.common.utils.SpringUtils;
import com.chiu.megalith.coop.dto.MessageDto;
import com.chiu.megalith.coop.mq.handler.CoopHandler;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.core.AcknowledgeMode;
import org.springframework.amqp.core.Queue;
import org.springframework.amqp.rabbit.connection.ConnectionFactory;
import org.springframework.amqp.rabbit.listener.SimpleMessageListenerContainer;
import org.springframework.amqp.rabbit.listener.adapter.MessageListenerAdapter;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

import java.util.Map;

@Slf4j
@Component
public class CoopMessageRabbitListener {

    private static class CacheHandlers {
        private static final Map<String, CoopHandler> cacheHandlers = SpringUtils.getHandlers(CoopHandler.class);
    }


    @Bean("CoopMessageListener")
    //processMessage作为listener
    public MessageListenerAdapter coopMessageListener(CoopMessageRabbitListener coopMessageRabbitListener) {
        return new MessageListenerAdapter(coopMessageRabbitListener, "processMessage");
    }

    //在container内将queue和listener绑定
    @Bean("CoopMessageListenerContainer")
    public SimpleMessageListenerContainer coopMessageListenerContainer(ConnectionFactory connectionFactory,
                                                                     @Qualifier("CoopMessageListener") MessageListenerAdapter listenerAdapter,
                                                                     @Qualifier("COOP_QUEUE") Queue queue) {
        SimpleMessageListenerContainer container = new SimpleMessageListenerContainer();
        listenerAdapter.containerAckMode(AcknowledgeMode.AUTO);
        container.setConcurrency("5-10");
        container.setConnectionFactory(connectionFactory);
        container.setQueueNames(queue.getName());
        container.setMessageListener(listenerAdapter);
        return container;
    }


    @SuppressWarnings("unused")
    public void processMessage(MessageDto msg) {
        for (CoopHandler handler : CacheHandlers.cacheHandlers.values()) {
            if (handler.supports(msg)) {
                handler.handle(msg);
                break;
            }
        }
    }
}
