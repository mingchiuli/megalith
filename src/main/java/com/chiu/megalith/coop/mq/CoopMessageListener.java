package com.chiu.megalith.coop.mq;

import com.chiu.megalith.infra.utils.SpringUtils;
import com.chiu.megalith.coop.dto.MessageDto;
import lombok.RequiredArgsConstructor;
import org.springframework.amqp.core.AcknowledgeMode;
import org.springframework.amqp.core.Queue;
import org.springframework.amqp.rabbit.connection.ConnectionFactory;
import org.springframework.amqp.rabbit.listener.SimpleMessageListenerContainer;
import org.springframework.amqp.rabbit.listener.adapter.MessageListenerAdapter;
import org.springframework.amqp.support.converter.Jackson2JsonMessageConverter;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
@RequiredArgsConstructor
public class CoopMessageListener {

    private static class CacheHandlers {
        private static final Map<String, BaseCoopHandler> cacheHandlers = SpringUtils.getHandlers(BaseCoopHandler.class);
    }

    private final Jackson2JsonMessageConverter jsonMessageConverter;


    @Bean("CoopMessageListener")
    public MessageListenerAdapter coopMessageListener(CoopMessageListener coopMessageListener) {
        return new MessageListenerAdapter(coopMessageListener, "processMessage");
    }

    @Bean("CoopMessageListenerContainer")
    public SimpleMessageListenerContainer coopMessageListenerContainer(ConnectionFactory connectionFactory,
                                                                       @Qualifier("CoopMessageListener") MessageListenerAdapter listenerAdapter,
                                                                       @Qualifier("COOP_QUEUE") Queue queue) {
        SimpleMessageListenerContainer container = new SimpleMessageListenerContainer();
        listenerAdapter.containerAckMode(AcknowledgeMode.AUTO);
        listenerAdapter.setMessageConverter(jsonMessageConverter);
        container.setConcurrency("5-10");
        container.setConnectionFactory(connectionFactory);
        container.setQueueNames(queue.getName());
        container.setMessageListener(listenerAdapter);
        return container;
    }


    @SuppressWarnings("unused")
    public void processMessage(MessageDto msg) {
        for (BaseCoopHandler handler : CacheHandlers.cacheHandlers.values()) {
            if (handler.supports(msg)) {
                handler.handle(msg);
                break;
            }
        }
    }
}
