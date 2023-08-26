package org.chiu.megalith.coop.vo;

import org.chiu.megalith.blog.entity.BlogEntity;
import lombok.Builder;
import lombok.Data;

/**
 * @author mingchiuli
 * @create 2023-01-14 1:55 am
 */
@Data
@Builder
public class InitCoopVo {

    private BlogEntity blogEntity;

    private UserEntityVo userEntityVo;
}
