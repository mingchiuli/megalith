package org.chiu.megalith.user.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import lombok.SneakyThrows;
import org.chiu.megalith.infra.code.CodeFactory;
import org.chiu.megalith.blog.http.OssHttpService;
import org.chiu.megalith.infra.utils.OssSignUtils;
import org.chiu.megalith.user.convertor.UserEntityVoConvertor;
import org.chiu.megalith.user.entity.UserEntity;
import org.chiu.megalith.user.repository.UserRepository;
import org.chiu.megalith.user.service.UserService;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.page.PageAdapter;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.user.vo.UserEntityVo;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.web.server.header.CacheControlServerHttpHeadersWriter;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.TimeUnit;

import static org.chiu.megalith.infra.lang.Const.*;
import static org.chiu.megalith.infra.lang.ExceptionMessage.*;

/**
 * @author mingchiuli
 * @create 2022-12-04 4:55 pm
 */
@Service
@RequiredArgsConstructor
public class UserServiceImpl implements UserService {

    private final UserRepository userRepository;

    private final StringRedisTemplate redisTemplate;

    private final OssHttpService ossHttpService;

    private final OssSignUtils ossSignUtils;

    private final CodeFactory codeFactory;

    private final ObjectMapper objectMapper;

    @Value("${blog.oss.base-url}")
    private String baseUrl;

    @Value("${blog.register.page-prefix}")
    private String pagePrefix;

    @Override
    public void updateLoginTime(String username, LocalDateTime time) {
        userRepository.updateLoginTime(username, time);
    }

    @Override
    public UserEntityVo findById(Long userId) {
        UserEntity userEntity = userRepository.findById(userId)
                .orElseThrow(() -> new MissException(USER_NOT_EXIST));

        return UserEntityVoConvertor.convert(userEntity);
    }

    @Override
    public void changeUserStatusByUsername(String username, Integer status) {
        userRepository.setUserStatusByUsername(username, status);
    }

    @Override
    public PageAdapter<UserEntityVo> listPage(Integer currentPage, Integer size) {
        var pageRequest = PageRequest.of(currentPage - 1,
                size,
                Sort.by("created").ascending());
        Page<UserEntity> page = userRepository.findAll(pageRequest);

        return UserEntityVoConvertor.convert(page);
    }

    @Override
    public void deleteUsers(List<Long> ids) {
        userRepository.deleteAllById(ids);
    }

    @Override
    public List<Long> findIdsByStatus(Integer status) {
        return userRepository.findByStatus(status);
    }

    @Override
    public UserEntityVo findByEmail(String email) {
        UserEntity userEntity = userRepository.findByEmail(email)
                .orElseThrow(() -> new MissException(EMAIL_NOT_EXIST));

        return UserEntityVoConvertor.convert(userEntity);
    }

    @Override
    public UserEntityVo findByPhone(String loginSMS) {
        UserEntity userEntity = userRepository.findByPhone(loginSMS)
                .orElseThrow(() -> new MissException(SMS_NOT_EXIST));

        return UserEntityVoConvertor.convert(userEntity);
    }

    @Override
    public String getRegisterPage(String username) {
        String token = UUID.randomUUID().toString();
        redisTemplate.opsForValue().set(REGISTER_PREFIX.getInfo() + token, username, 1, TimeUnit.HOURS);
        if (StringUtils.hasLength(username)) {
            return pagePrefix + token + "?username=" + username;
        }
        return pagePrefix + token;
    }

    @SneakyThrows
    @Override
    public String imageUpload(String token, MultipartFile image) {
        Boolean exist = redisTemplate.hasKey(REGISTER_PREFIX.getInfo() + token);
        if (Objects.isNull(exist) || Boolean.FALSE.equals(exist)) {
            throw new BadCredentialsException(NO_AUTH.getMsg());
        }

        Assert.notNull(image, UPLOAD_MISS.getMsg());
        String uuid = UUID.randomUUID().toString();
        String originalFilename = image.getOriginalFilename();
        originalFilename = Optional.ofNullable(originalFilename)
                .orElseGet(() -> UUID.randomUUID().toString())
                .replace(" ", "");
        String objectName = "avatar/" + uuid + "-" + originalFilename;
        byte[] imageBytes = image.getBytes();

        Map<String, String> headers = new HashMap<>();
        String gmtDate = ossSignUtils.getGMTDate();
        headers.put(HttpHeaders.DATE, gmtDate);
        headers.put(HttpHeaders.AUTHORIZATION, ossSignUtils.getAuthorization(objectName, HttpMethod.PUT.name(), "image/jpg"));
        headers.put(HttpHeaders.CACHE_CONTROL, CacheControlServerHttpHeadersWriter.PRAGMA_VALUE);
        headers.put(HttpHeaders.CONTENT_TYPE, "image/jpg");
        ossHttpService.putOssObject(objectName, imageBytes, headers);
        return baseUrl + "/" + objectName;
    }

    @Override
    public void imageDelete(String token, String url) {
        Boolean exist = redisTemplate.hasKey(REGISTER_PREFIX.getInfo() + token);
        if (Objects.isNull(exist) || Boolean.FALSE.equals(exist)) {
            throw new BadCredentialsException(NO_AUTH.getMsg());
        }
        String objectName = url.replace(baseUrl + "/", "");
        Map<String, String> headers = new HashMap<>();
        String gmtDate = ossSignUtils.getGMTDate();
        headers.put(HttpHeaders.DATE, gmtDate);
        headers.put(HttpHeaders.AUTHORIZATION, ossSignUtils.getAuthorization(objectName, HttpMethod.DELETE.name(), ""));
        ossHttpService.deleteOssObject(objectName, headers);
    }

    @Override
    public Boolean checkRegisterPage(String token) {
        return redisTemplate.hasKey(REGISTER_PREFIX.getInfo() + token);
    }

    @SneakyThrows
    @Override
    public void download(HttpServletResponse response) {
        ServletOutputStream outputStream = response.getOutputStream();
        response.setCharacterEncoding(StandardCharsets.UTF_8.name());

        List<UserEntity> users = userRepository.findAll();
        byte[] bytes = objectMapper.writeValueAsBytes(users);
        outputStream.write(bytes);
        outputStream.flush();
        outputStream.close();
    }
}
