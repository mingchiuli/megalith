package com.chiu.megalith.common.config;

import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.amqp.support.converter.Jackson2JsonMessageConverter;
import org.springframework.context.annotation.Configuration;
import org.springframework.retry.policy.CircuitBreakerRetryPolicy;
import org.springframework.retry.policy.SimpleRetryPolicy;
import org.springframework.retry.support.RetryTemplate;

/**
 * @author mingchiuli
 * @create 2022-12-23 12:32 pm
 */
@Configuration(proxyBeanMethods = false)
@RequiredArgsConstructor
@Slf4j
public class RabbitTemplateConfig {

    private final RabbitTemplate rabbitTemplate;

    private final Jackson2JsonMessageConverter jsonMessageConverter;

    @PostConstruct
    public void initRabbitTemplate() {
        //设置抵达broker服务器的回掉
        //当前消息的唯一关联数据、服务器对消息是否成功收到、失败的原因
        rabbitTemplate.setConfirmCallback((correlationData, ack, cause) -> log.info("message not come to mq {}, {}, {}", correlationData, ack, cause));

        //设置抵达消息队列的确认回调
        //只要消息没有投递给指定的队列，就触发这个失败回调
        rabbitTemplate.setReturnsCallback(returned -> log.info("message not come to queue {}", returned));

        rabbitTemplate.setMessageConverter(jsonMessageConverter);

        RetryTemplate retryTemplate = new RetryTemplate();
        CircuitBreakerRetryPolicy circuitBreakerRetryPolicy = new CircuitBreakerRetryPolicy(new SimpleRetryPolicy(10));
        circuitBreakerRetryPolicy.setOpenTimeout(5000L);
        circuitBreakerRetryPolicy.setResetTimeout(10000L);
        retryTemplate.setRetryPolicy(circuitBreakerRetryPolicy);

        rabbitTemplate.setRetryTemplate(retryTemplate);
    }
}
