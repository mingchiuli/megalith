package com.chiu.megalith.websocket.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author mingchiuli
 * @create 2022-12-25 6:55 pm
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UserEntityVo {
    private Long id;

    private String username;

    private String avatar;

    private Integer orderNumber;

    private String serverMark;
}
