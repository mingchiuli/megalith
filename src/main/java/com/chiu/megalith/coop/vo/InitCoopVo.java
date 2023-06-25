package com.chiu.megalith.coop.vo;

import com.chiu.megalith.blog.entity.BlogEntity;
import lombok.Builder;
import lombok.Data;

import java.io.Serializable;
import java.util.Collection;

/**
 * @author mingchiuli
 * @create 2023-01-14 1:55 am
 */
@Data
@Builder
public class InitCoopVo implements Serializable {

    private BlogEntity blogEntity;

    private Collection<String> userEntityVos;
}
