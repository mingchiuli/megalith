package com.chiu.megalith.websocket.dto.impl;

import com.chiu.megalith.websocket.dto.Container;
import com.chiu.megalith.websocket.dto.MessageDto;
import com.chiu.megalith.websocket.vo.UserEntityVo;
import lombok.*;

import java.io.Serializable;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class InitDto implements Serializable, MessageDto {
    private Container<Bind> data;

    @Override
    @SuppressWarnings("unchecked")
    public Container<Bind> getData() {
        return data;
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class Bind implements Serializable {
        private Long blogId;
        private List<UserEntityVo> users;
    }
}
