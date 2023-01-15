package com.chiu.megalith.websocket.mq;

import com.chiu.megalith.common.utils.SpringUtils;
import com.chiu.megalith.websocket.dto.MessageDto;
import com.chiu.megalith.websocket.mq.handler.WSHandler;
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
public class WSMessageRabbitListener {

    private static class CacheHandlers {
        private static final Map<String, WSHandler> cacheHandlers = SpringUtils.getHandlers(WSHandler.class);
    }


    @Bean("CoopMessageListener")
    //processMessage作为listener
    public MessageListenerAdapter wSMessageListener(WSMessageRabbitListener wSMessageRabbitListener) {
        return new MessageListenerAdapter(wSMessageRabbitListener, "processMessage");
    }

    //在container内将queue和listener绑定
    @Bean("CoopMessageListenerContainer")
    public SimpleMessageListenerContainer wSMessageListenerContainer(ConnectionFactory connectionFactory,
                                                                     @Qualifier("CoopMessageListener") MessageListenerAdapter listenerAdapter,
                                                                     @Qualifier("COOP_QUEUE") Queue queue) {
        SimpleMessageListenerContainer container = new SimpleMessageListenerContainer();
        listenerAdapter.containerAckMode(AcknowledgeMode.AUTO);
        container.setConnectionFactory(connectionFactory);
        container.setQueueNames(queue.getName());
        container.setMessageListener(listenerAdapter);
        return container;
    }


    @SuppressWarnings("unused")
    public void processMessage(MessageDto msg) {
        for (WSHandler handler : CacheHandlers.cacheHandlers.values()) {
            if (handler.supports(msg)) {
                handler.handle(msg);
                break;
            }
        }
    }
}
