package com.chiu.megalith.websocket.controller;

import com.chiu.megalith.common.lang.Result;
import com.chiu.megalith.common.valid.CoopBlogId;
import com.chiu.megalith.websocket.service.InitCoopService;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;
import java.util.Map;


/**
 * @author mingchiuli
 * @create 2022-12-25 7:14 pm
 */
@RestController
@RequiredArgsConstructor
public class InitCoopController {

    private final InitCoopService initCoopService;

    @GetMapping("/room/init/{blogId}/{number}")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public Result<Map<String, Object>> initCoop(@PathVariable @CoopBlogId Long blogId, @PathVariable Integer orderNumber) {
        Map<String, Object> map = initCoopService.initCoop(blogId, orderNumber);
        return Result.success(map);
    }
}
