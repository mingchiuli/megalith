package com.chiu.megalith.search.mq;

import com.chiu.megalith.common.search.BlogSearchIndexMessage;
import com.chiu.megalith.common.utils.SpringUtils;
import com.chiu.megalith.search.config.ESMQConfig;
import com.chiu.megalith.search.mq.handler.BlogIndexAbstractHandler;
import com.rabbitmq.client.Channel;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.stereotype.Component;

import java.util.Map;

/**
 * @author mingchiuli
 * @create 2021-12-13 11:38 AM
 */
@Slf4j
@Component
public class BlogRabbitListener {
    private static class CacheHandlers {
        private static final Map<String, BlogIndexAbstractHandler> cacheHandlers = SpringUtils.getHandlers(BlogIndexAbstractHandler.class);
    }

    @RabbitListener(queues = ESMQConfig.ES_QUEUE)
    public void handler(BlogSearchIndexMessage message, Channel channel, Message msg) {
        for (BlogIndexAbstractHandler handler : CacheHandlers.cacheHandlers.values()) {
            if (handler.supports(message.typeEnum)) {
                handler.handle(message, channel, msg);
                break;
            }
        }
    }
}
