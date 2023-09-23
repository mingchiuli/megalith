package org.chiu.megalith.coop.dto;

import lombok.Builder;
import lombok.Data;


/**
 * @author mingchiuli
 * @create 2022-12-25 6:55 pm
 */
@Data
@Builder
public class UserEntityDto {

    private Long id;

    private String nickname;

    private String avatar;

    private String nodeMark;
}
