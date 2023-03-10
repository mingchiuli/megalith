package com.chiu.megalith.coop.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * @author mingchiuli
 * @create 2022-12-25 6:55 pm
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UserEntityVo implements Serializable {
    private Long id;

    private String username;

    private String avatar;

    private String nodeMark;
}
