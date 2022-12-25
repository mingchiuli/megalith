package com.chiu.megalith.websocket.dto.impl;

import com.chiu.megalith.manage.vo.UserEntityVo;
import com.chiu.megalith.websocket.dto.Container;
import com.chiu.megalith.websocket.dto.MessageDto;
import lombok.AllArgsConstructor;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

@Data
@AllArgsConstructor
public class DestroyDto implements Serializable, MessageDto<DestroyDto.Bind> {
    private Container<Bind> data;

    @Override
    public Container<Bind> getData() {
        return data;
    }

    @Data
    @AllArgsConstructor
    public static class Bind implements Serializable {
        Long blogId;
        List<UserEntityVo> users;
    }
}
