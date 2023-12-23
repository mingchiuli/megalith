package org.chiu.megalith.infra.utils;

import lombok.SneakyThrows;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.Base64;
import java.util.Calendar;
import java.util.Locale;
import java.util.TimeZone;

@Component
public class OssSignUtils {

    @Value("${blog.oss.access-key-id}")
    private String accessKeyId;

    @Value("${blog.oss.access-key-secret}")
    private String accessKeySecret;

    @Value("${blog.oss.bucket-name}")
    private String bucketName;

    private final static String ALGORITHM = "HmacSHA1";

    @SneakyThrows
    private byte[] hmacSha1(String data, String accessKeySecret) {
        Mac mac = Mac.getInstance(ALGORITHM);
        SecretKeySpec keySpec = new SecretKeySpec(accessKeySecret.getBytes(), ALGORITHM);
        mac.init(keySpec);
        return mac.doFinal(data.getBytes(StandardCharsets.UTF_8));
    }

    public String getGMTDate() {
        Calendar cd = Calendar.getInstance();
        SimpleDateFormat sdf = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss 'GMT'", Locale.UK);
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        return sdf.format(cd.getTime());
    }

    private String buildSignData(String date, String canonicalizedResource, String methodName, String contentType) {
        //https://help.aliyun.com/zh/oss/developer-reference/include-signatures-in-the-authorization-header?spm=a2c4g.11186623.0.0.54e828efd3PoE6
        return  methodName + "\n" +
                "\n" +
                contentType + "\n" +
                date + "\n" +
                canonicalizedResource;
    }

    public String getAuthorization(String objectName, String method, String contentType) {
        String date = getGMTDate();
        String signData = buildSignData(date, "/" + bucketName + "/" + objectName, method, contentType);
        byte[] bytes = hmacSha1(signData, accessKeySecret);
        String signature = Base64.getEncoder().encodeToString(bytes);
        return  "OSS " + accessKeyId + ":" + signature;
    }
}