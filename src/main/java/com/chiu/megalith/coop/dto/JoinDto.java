package com.chiu.megalith.coop.dto;

import com.chiu.megalith.coop.vo.UserEntityVo;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.io.Serializable;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class JoinDto extends MessageDto implements Serializable {

    @EqualsAndHashCode(callSuper = true)
    @Data
    @SuperBuilder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Bind extends BaseBind implements Serializable {
        private UserEntityVo user;
    }
}
