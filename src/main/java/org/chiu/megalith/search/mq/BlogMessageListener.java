package org.chiu.megalith.search.mq;

import lombok.extern.slf4j.Slf4j;
import org.chiu.megalith.infra.search.BlogSearchIndexMessage;
import org.chiu.megalith.search.config.ElasticSearchRabbitConfig;
import org.chiu.megalith.search.mq.handler.BlogIndexSupport;
import com.rabbitmq.client.Channel;
import lombok.RequiredArgsConstructor;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2021-12-13 11:38 AM
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class BlogMessageListener {

    private final List<BlogIndexSupport> elasticsearchHandlers;

    @RabbitListener(queues = ElasticSearchRabbitConfig.ES_QUEUE, 
            concurrency = "10",
            messageConverter = "jsonMessageConverter",
            executor = "mqExecutor")
    public void handler(BlogSearchIndexMessage message, Channel channel, Message msg) {
        for (BlogIndexSupport handler : elasticsearchHandlers) {
            if (handler.supports(message.getTypeEnum())) {
                handler.handle(message, channel, msg);
                break;
            }
        }
    }
}
