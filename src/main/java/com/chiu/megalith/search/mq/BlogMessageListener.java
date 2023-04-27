package com.chiu.megalith.search.mq;

import com.chiu.megalith.infra.search.BlogSearchIndexMessage;
import com.chiu.megalith.search.config.ElasticSearchRabbitConfig;
import com.chiu.megalith.search.mq.handler.BlogIndexSupport;
import com.rabbitmq.client.Channel;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2021-12-13 11:38 AM
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class BlogMessageListener {

    private final List<BlogIndexSupport> elasticsearchHandlers;

    @RabbitListener(
            queues = ElasticSearchRabbitConfig.ES_QUEUE,
            concurrency = "5-10",
            messageConverter = "jsonMessageConverter")
    public void handler(BlogSearchIndexMessage message, Channel channel, Message msg) {
        for (BlogIndexSupport handler : elasticsearchHandlers) {
            if (handler.supports(message.typeEnum)) {
                handler.handle(message, channel, msg);
                break;
            }
        }
    }
}
