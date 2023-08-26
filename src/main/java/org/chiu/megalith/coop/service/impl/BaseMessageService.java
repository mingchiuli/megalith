package org.chiu.megalith.coop.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.chiu.megalith.coop.config.CoopRabbitConfig;
import org.chiu.megalith.coop.dto.impl.FinishCoopDto;
import org.chiu.megalith.coop.vo.UserEntityVo;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.utils.JsonUtils;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.StringRedisTemplate;


public class BaseMessageService {

    protected final StringRedisTemplate redisTemplate;

    protected final JsonUtils jsonUtils;

    protected final RabbitTemplate rabbitTemplate;

    public BaseMessageService(StringRedisTemplate redisTemplate,
                              JsonUtils jsonUtils,
                              RabbitTemplate rabbitTemplate) {
        this.redisTemplate = redisTemplate;
        this.jsonUtils = jsonUtils;
        this.rabbitTemplate = rabbitTemplate;                            

    }

    protected void sendToOtherUsers(Long blogId, Long userId) {
        HashOperations<String, String, String> operations = redisTemplate.opsForHash();
        Set<String> nodeMarkSet = new HashSet<>();
        Map<String, List<Long>> idMarkMap = new HashMap<>();
        operations.entries(Const.COOP_PREFIX.getInfo() + blogId)
                .forEach((key, value) -> {
                    UserEntityVo userVo = jsonUtils.readValue(value, UserEntityVo.class);
                    String nodeMark = userVo.getNodeMark();
                    List<Long> idList = idMarkMap.getOrDefault(nodeMark, new ArrayList<>());
                    idList.add(userVo.getId());
                    if (Boolean.FALSE.equals(Objects.equals(Const.BLOG_CONTENT.getInfo(), key)) 
                        && Boolean.FALSE.equals(Objects.equals(userVo.getId(), userId))) {
                        nodeMarkSet.add(nodeMark);
                    }
                }); 
        
        nodeMarkSet.forEach(mark -> {
            var dto = new FinishCoopDto();
            dto.setBlogId(blogId);
            dto.setFromId(userId);
            List<Long> idList = idMarkMap.get(mark);
            dto.setToId(idList);
            rabbitTemplate.convertAndSend(CoopRabbitConfig.WS_TOPIC_EXCHANGE,
                    CoopRabbitConfig.WS_BINDING_KEY + mark,
                    dto);
            
        });
    }
}
