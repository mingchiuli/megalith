package com.chiu.megalith.websocket.dto.impl;

import com.chiu.megalith.websocket.dto.Container;
import com.chiu.megalith.websocket.dto.MessageDto;
import com.chiu.megalith.websocket.vo.UserEntityVo;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class InitDto implements Serializable, MessageDto<InitDto.Bind> {
    private Container<Bind> data;

    @Override
    public Container<Bind> getData() {
        return data;
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class Bind implements Serializable {
        Long blogId;
        List<UserEntityVo> users;
    }
}
