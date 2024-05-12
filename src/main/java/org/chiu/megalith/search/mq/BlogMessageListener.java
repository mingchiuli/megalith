package org.chiu.megalith.search.mq;

import org.chiu.megalith.infra.constant.BlogOperateMessage;
import org.chiu.megalith.search.config.ElasticSearchRabbitConfig;
import com.rabbitmq.client.Channel;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.search.mq.handler.BlogIndexSupport;
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
public class BlogMessageListener {

    private final List<BlogIndexSupport> elasticsearchHandlers;

    @RabbitListener(queues = ElasticSearchRabbitConfig.ES_QUEUE, 
            concurrency = "10",
            messageConverter = "jsonMessageConverter",
            executor = "mqExecutor")
    public void handler(BlogOperateMessage message, Channel channel, Message msg) {
        for (BlogIndexSupport handler : elasticsearchHandlers) {
            if (handler.supports(message.getTypeEnum())) {
                handler.handle(message, channel, msg);
                break;
            }
        }
    }
}
