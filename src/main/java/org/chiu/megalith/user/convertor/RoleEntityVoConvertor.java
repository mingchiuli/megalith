package org.chiu.megalith.user.convertor;

import org.chiu.megalith.authority.entity.RoleEntity;
import org.chiu.megalith.authority.vo.RoleEntityVo;

import java.util.ArrayList;
import java.util.List;

/**
 * @Author limingjiu
 * @Date 2024/5/11 10:11
 **/
public class RoleEntityVoConvertor {

    public static List<RoleEntityVo> convert(List<RoleEntity> entities) {
        List<RoleEntityVo> vos = new ArrayList<>();
        entities.forEach(item -> vos
                .add(RoleEntityVo.builder()
                        .code(item.getCode())
                        .id(item.getId())
                        .status(item.getStatus())
                        .name(item.getName())
                        .build()));
        return vos;

    }

}
