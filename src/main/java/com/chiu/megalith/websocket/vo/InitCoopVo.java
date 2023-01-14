package com.chiu.megalith.websocket.vo;

import com.chiu.megalith.blog.entity.BlogEntity;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2023-01-14 1:55 am
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class InitCoopVo {

    private BlogEntity blogEntity;

    private List<UserEntityVo> userEntityVos;
}